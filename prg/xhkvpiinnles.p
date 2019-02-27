&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : xhkvpiinnles.p
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
DEF INPUT  PARAMETER lFilId      AS DEC    NO-UNDO.
DEF INPUT  PARAMETER h_Parent    AS HANDLE NO-UNDO.
DEF OUTPUT PARAMETER iAntLinjer  AS INT    NO-UNDO.

DEF VAR iTotAntLinjer AS INT  NO-UNDO.
DEF VAR cLinje        AS CHAR NO-UNDO.
DEF VAR cFilNavn      AS CHAR NO-UNDO.
DEF VAR bHk           AS LOG  NO-UNDO.

DEF VAR cType          AS CHAR NO-UNDO.
DEF VAR iRecType       AS INT  NO-UNDO. 
DEF VAR cTabellNavn    AS CHAR NO-UNDO. 
DEF VAR cRecVersion    AS CHAR NO-UNDO.
DEF VAR iAntRecord     AS INT  NO-UNDO.
DEF VAR cTblLst        AS CHAR NO-UNDO.
DEF VAR cTekst         AS CHAR NO-UNDO.
DEF VAR bHKDirekteOppd AS LOG  NO-UNDO.
DEFINE VARIABLE lTid      AS INTEGER   NO-UNDO.
DEFINE VARIABLE bLogg AS LOG NO-UNDO.

/* --- tmp tabeller --- */

DEFINE TEMP-TABLE TT_Farg             NO-UNDO LIKE Farg
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Feilkode         NO-UNDO LIKE Feilkode
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Foder            NO-UNDO LIKE Foder
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Handtering       NO-UNDO LIKE Handtering
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_HuvGr            NO-UNDO LIKE HuvGr
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_InnerSula        NO-UNDO LIKE InnerSula
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Kategori         NO-UNDO LIKE Kategori
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Klack            NO-UNDO LIKE Klack
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Kravkode         NO-UNDO LIKE Kravkode
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Last-Sko         NO-UNDO LIKE Last-Sko
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Material         NO-UNDO LIKE Material
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Moms             NO-UNDO LIKE Moms
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Ovandel          NO-UNDO LIKE Ovandel
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Post             NO-UNDO LIKE Post
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Prisgruppe       NO-UNDO LIKE Prisgruppe
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Prisprofil       NO-UNDO LIKE Prisprofil
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Produsent        NO-UNDO LIKE Produsent
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Prov             NO-UNDO LIKE Prov
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Rabatt           NO-UNDO LIKE Rabatt
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_SaSong           NO-UNDO LIKE SaSong
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Slitsula         NO-UNDO LIKE Slitsula
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_StrKonv          NO-UNDO LIKE StrKonv
  FIELD RecType AS INT
  INDEX Dummy1 IS PRIMARY StrKode 
  INDEX Dummy2 Storl.
DEFINE TEMP-TABLE TT_StrType          NO-UNDO LIKE StrType
  FIELD RecType AS INTEGER INDEX RecType RecType.
DEFINE TEMP-TABLE TT_StrTStr          NO-UNDO LIKE StrTStr
  FIELD RecType AS INTEGER INDEX RecType RecType.
DEFINE TEMP-TABLE TT_Varemerke        NO-UNDO LIKE Varemerke
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_VarGr            NO-UNDO LIKE VarGr
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_VgKat            NO-UNDO LIKE VgKat
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_LevSort          NO-UNDO LIKE LevSort
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_LevSAnt          NO-UNDO LIKE LevSAnt
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Kjede            NO-UNDO LIKE Kjede
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_KjedeDistrikt    NO-UNDO LIKE KjedeDistrikt
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_KjedeRegion      NO-UNDO LIKE KjedeRegion
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_KjedensButikker  NO-UNDO LIKE KjedensButikker
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_KasValuta        NO-UNDO LIKE KasValuta
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Valuta           NO-UNDO LIKE Valuta
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_VPIBildeData     NO-UNDO LIKE VPIBildeData
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_VPIBildeRegister NO-UNDO LIKE VPIBildeRegister
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_VPIErstattningsvare NO-UNDO LIKE VPIErstattningsvare
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_VPIPakkelinje    NO-UNDO LIKE VPIPakkelinje
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_ProgramListe     NO-UNDO LIKE ProgramListe
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tt_UtbetType        NO-UNDO LIKE UtbetType
  FIELD RecType AS INT.

DEFINE TEMP-TABLE tmpTT_Farg             NO-UNDO LIKE Farg
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Feilkode         NO-UNDO LIKE Feilkode
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Foder            NO-UNDO LIKE Foder
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Handtering       NO-UNDO LIKE Handtering
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_HuvGr            NO-UNDO LIKE HuvGr
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_InnerSula        NO-UNDO LIKE InnerSula
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Kategori         NO-UNDO LIKE Kategori
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Klack            NO-UNDO LIKE Klack
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Kravkode         NO-UNDO LIKE Kravkode
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Last-Sko         NO-UNDO LIKE Last-Sko
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Material         NO-UNDO LIKE Material
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Moms             NO-UNDO LIKE Moms
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Ovandel          NO-UNDO LIKE Ovandel
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Post             NO-UNDO LIKE Post
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Prisgruppe       NO-UNDO LIKE Prisgruppe
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Prisprofil       NO-UNDO LIKE Prisprofil
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Produsent        NO-UNDO LIKE Produsent
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Prov             NO-UNDO LIKE Prov
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Rabatt           NO-UNDO LIKE Rabatt
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_SaSong           NO-UNDO LIKE SaSong
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Slitsula         NO-UNDO LIKE Slitsula
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_StrKonv          NO-UNDO LIKE StrKonv
  FIELD RecType AS INT
  INDEX Dummy1 IS PRIMARY StrKode 
  INDEX Dummy2 Storl.
DEFINE TEMP-TABLE tmpTT_StrType          NO-UNDO LIKE StrType
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_StrTStr          NO-UNDO LIKE StrTStr
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Varemerke        NO-UNDO LIKE Varemerke
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_VarGr            NO-UNDO LIKE VarGr
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_VgKat            NO-UNDO LIKE VgKat
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_LevSort          NO-UNDO LIKE LevSort
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_LevSAnt          NO-UNDO LIKE LevSAnt
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Kjede            NO-UNDO LIKE Kjede
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_KjedeDistrikt    NO-UNDO LIKE KjedeDistrikt
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_KjedeRegion      LIKE KjedeRegion
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_KjedensButikker  NO-UNDO LIKE KjedensButikker
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_KasValuta        NO-UNDO LIKE KasValuta
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_Valuta           NO-UNDO LIKE Valuta
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_VPIBildeData     NO-UNDO LIKE VPIBildeData
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_VPIBildeRegister NO-UNDO LIKE VPIBildeRegister
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_VPIErstattningsvare NO-UNDO LIKE VPIErstattningsvare
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmpTT_VPIPakkelinje    NO-UNDO LIKE VPIPakkelinje
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmptt_ProgramListe     NO-UNDO LIKE ProgramListe
  FIELD RecType AS INT.
DEFINE TEMP-TABLE tmptt_UtbetType        NO-UNDO LIKE UtbetType
  FIELD RecType AS INT.

/* TN 4/9-09
DEFINE TEMP-TABLE tmptt_LevKontakt       NO-UNDO LIKE LevKontakt
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_LevKontakt          NO-UNDO LIKE LevKontakt
  FIELD RecType AS INT.
*/

DEFINE TEMP-TABLE tmpTT_VareBehType      NO-UNDO LIKE VareBehType
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_VareBehType         NO-UNDO LIKE VareBehType
  FIELD RecType AS INT.

DEFINE TEMP-TABLE tmpTT_VareBokType      NO-UNDO LIKE VareBokType
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_VareBokType         NO-UNDO LIKE VareBokType
  FIELD RecType AS INT.

DEFINE TEMP-TABLE tmpTT_Messe            NO-UNDO LIKE Messe
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Messe               NO-UNDO LIKE Messe
  FIELD RecType AS INT.

DEFINE TEMP-TABLE tmpTT_LokalGruppering  NO-UNDO LIKE LokalGruppering
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_LokalGruppering     NO-UNDO LIKE LokalGruppering
  FIELD RecType AS INT.

DEFINE TEMP-TABLE tmpTT_IndType          NO-UNDO LIKE IndType
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_IndType             NO-UNDO LIKE IndType
  FIELD RecType AS INT.

DEFINE TEMP-TABLE tmpTT_InnBetType       NO-UNDO LIKE InnBetType
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_InnBetType          NO-UNDO LIKE InnBetType
  FIELD RecType AS INT.

DEFINE TEMP-TABLE tmpTT_Garanti          NO-UNDO LIKE Garanti
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Garanti             NO-UNDO LIKE Garanti
  FIELD RecType AS INT.


DEFINE TEMP-TABLE tmpTT_GaveKType        NO-UNDO LIKE GaveKType
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_GaveKType           NO-UNDO LIKE GaveKType
  FIELD RecType AS INT.

DEFINE TEMP-TABLE tmpTT_DriftsForm       NO-UNDO LIKE DriftsForm
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_DriftsForm          NO-UNDO LIKE DriftsForm
  FIELD RecType AS INT.

DEFINE TEMP-TABLE tmpTT_DriftsType       NO-UNDO LIKE DriftsType
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_DriftsType          NO-UNDO LIKE DriftsType
  FIELD RecType AS INT.

DEFINE TEMP-TABLE tmpTT_Aktivitet        NO-UNDO LIKE Aktivitet
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Aktivitet          NO-UNDO LIKE Aktivitet
  FIELD RecType AS INT.

DEFINE TEMP-TABLE tmpTT_Beliggenhet     NO-UNDO LIKE Beliggenhet
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_Beliggenhet          NO-UNDO LIKE Beliggenhet
  FIELD RecType AS INT.

DEFINE TEMP-TABLE tmpTT_VPIAltLevBas    NO-UNDO LIKE VPIAltLevBas
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_VPIAltLevBas          NO-UNDO LIKE VPIAltLevBas
  FIELD RecType AS INT.

/* TN 4/9-09
DEFINE TEMP-TABLE tmpTT_VPIArtBestPkt     NO-UNDO LIKE VPIArtBestPkt
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_VPIArtBestPkt          NO-UNDO LIKE VPIArtBestPkt
  FIELD RecType AS INT.
*/

DEFINE TEMP-TABLE tmpTT_VgAkt NO-UNDO LIKE VgAkt
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_VgAkt NO-UNDO LIKE Vgakt
  FIELD RecType AS INT.

DEFINE TEMP-TABLE tmpTT_AltLevBas NO-UNDO LIKE AltLevBas
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_AltLevBas NO-UNDO LIKE AltLevBas
  FIELD RecType AS INT.

/* TN 4/9-09
DEFINE TEMP-TABLE tmpTT_ArtBestPkt     NO-UNDO LIKE ArtBestPkt
  FIELD RecType AS INT.
DEFINE TEMP-TABLE TT_ArtBestPkt          NO-UNDO LIKE ArtBestPkt
  FIELD RecType AS INT.
*/

DEFINE TEMP-TABLE tt_Error NO-UNDO 
  FIELD LinjeNr AS INT
  FIELD Tekst   AS CHAR
  .
/* -------------------- */

DEF NEW SHARED STREAM InnFil.

/* Definisjon av temp tabeller for de programmer som ligger som separate programmer */
{tmptt_VPIArtBas.i &NEW=NEW &SHARED=SHARED}
{tmptt_VPIArtPris.i &NEW=NEW &SHARED=SHARED}
{tmptt_VPISTrekkode.i &NEW=NEW &SHARED=SHARED}
{tmptt_Levbas.i &NEW=NEW &SHARED=SHARED}

{tmptt_Anv-Kod.i &NEW=NEW &SHARED=SHARED}
{tmptt_Avdeling.i &NEW=NEW &SHARED=SHARED}
{tmptt_DefaultLevDato.i &NEW=NEW &SHARED=SHARED}
{tmptt_Behandlingskode.i &NEW=NEW &SHARED=SHARED}
/* {tmptt_EkstVPILev.i &NEW=NEW &SHARED=SHARED} */

{windows.i}

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
         HEIGHT             = 33.14
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* Skriv til loggfil */
bLogg = FALSE.

/* HK installasjon. */
{syspara.i 1 1 18 cTekst}
IF CAN-DO("yes,1,ja,true",cTekst) THEN
    bHk = TRUE.
  ELSE
    bHk = FALSE.

/* Direkte oppdatering av prisendirnger og varetekster på HK varer. */
{syspara.i 50 16 1 cTekst}
IF CAN-DO("YES,TRUE,1,ja",cTekst) THEN
    bHKDirekteOppd = TRUE.
  ELSE
    bHKDirekteOppd = FALSE.

FIND VPIFilHode NO-LOCK WHERE
    VPIFilHode.FilId = lFilId NO-ERROR.
IF NOT AVAILABLE VPIFilHode THEN
DO:
    RETURN " ** Ukjent VPIFilHode post (" + STRING(lFilId) + ").".
END.
ASSIGN
    cFilNavn = VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.

RUN LesInnFil.

PROCEDURE ShellExecute{&A} EXTERNAL "shell32" :
     DEFINE INPUT PARAMETER HWND AS LONG.
     DEFINE INPUT PARAMETER lpOperation AS CHAR.
     DEFINE INPUT PARAMETER lpFile AS CHAR.
     DEFINE INPUT PARAMETER lpParameters AS CHAR.
     DEFINE INPUT PARAMETER lpDirectory AS CHAR.
     DEFINE INPUT PARAMETER nShowCmd AS LONG.
     DEFINE RETURN PARAMETER hInstance AS LONG.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ErrorLogg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ErrorLogg Procedure 
PROCEDURE ErrorLogg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE pcTekst AS CHARACTER NO-UNDO.

IF NOT CAN-FIND(FIRST tt_Error)
  THEN RETURN.

IF AVAILABLE VPIFilHode THEN 
  pcTekst = "Feil i fil: " + VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn.
ELSE
  pcTekst = "Ukjent/slettet VPI fil (xhkvpiinnles.p)". 
  
  OUTPUT TO VALUE("Error.Txt").
    PUT UNFORMATTED
      "Innlesning " + STRING(TODAY) + "  " + STRING(TIME,"HH:MM:SS") + "." SKIP
      pcTekst SKIP
      .
    FOR EACH tt_Error:
      PUT UNFORMATTED tt_Error.Tekst SKIP.
    END.
  OUTPUT CLOSE.
  IF SEARCH("Error.Txt") <> ? THEN
  DO:
    DEF VAR hInstance AS INT.

    RUN ShellExecute{&A} IN hpApi(0,
                                  "open",
                                  "notepad.exe",
                                  SEARCH("Error.Txt"),
                                  "",
                                  1,
                                  OUTPUT hInstance).

  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesAktivitet1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesAktivitet1.0 Procedure 
PROCEDURE lesAktivitet1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmptt_Aktivitet.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_Aktivitet.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_Aktivitet.AktNr          
      tmpTT_Aktivitet.Beskrivelse      
      tmpTT_Aktivitet.Merknad        
      tmpTT_Aktivitet.EDato         
      tmpTT_Aktivitet.ETid          
      tmpTT_Aktivitet.BrukerID      
      tmpTT_Aktivitet.RegistrertDato
      tmpTT_Aktivitet.RegistrertTid 
      tmpTT_Aktivitet.RegistrertAv
      .
    FIND TT_Aktivitet WHERE
        TT_Aktivitet.AktNr = tmpTT_Aktivitet.AktNr NO-ERROR.
    IF NOT AVAILABLE TT_Aktivitet THEN
        CREATE TT_Aktivitet.
    BUFFER-COPY tmpTT_Aktivitet TO TT_Aktivitet.

    RELEASE TT_Aktivitet.
    DELETE tmpTT_Aktivitet.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesAltLevBas1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesAltLevBas1.0 Procedure 
PROCEDURE lesAltLevBas1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmptt_AltLevBas.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_AltLevBas.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_AltLevBas.ArtikkelNr          
      tmpTT_AltLevBas.LevNr 
      tmpTT_AltLevBas.EDato         
      tmpTT_AltLevBas.ETid          
      tmpTT_AltLevBas.BrukerID      
      tmpTT_AltLevBas.RegistrertDato
      tmpTT_AltLevBas.RegistrertTid 
      tmpTT_AltLevBas.RegistrertAv
      .
    FIND TT_AltLevBas WHERE
        TT_AltLevBas.ArtikkelNr = tmpTT_AltLevBas.ArtikkelNr AND
        TT_AltLevBas.LevNr      = tmpTT_AltLevBas.LevNr NO-ERROR.
    IF CAN-FIND(ArtBas WHERE
                ArtBas.ArtikkelNr = tmpTT_AltLevBas.ArtikkelNr) THEN
    DO:
        IF NOT AVAILABLE TT_AltLevBas THEN
            CREATE TT_AltLevBas.
        BUFFER-COPY tmpTT_AltLevBas TO TT_AltLevBas.
    END.
    IF AVAILABLE TT_AltLevBas THEN
        RELEASE TT_AltLevBas.
    DELETE tmpTT_AltLevBas.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesAnv-Kod1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesAnv-Kod1.0 Procedure 
PROCEDURE lesAnv-Kod1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Anv-Kod.
    ASSIGN
      tmpTT_Anv-Kod.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_Anv-Kod.Anv-Id        
      tmpTT_Anv-Kod.AnvBeskr      
      tmpTT_Anv-Kod.EDato         
      tmpTT_Anv-Kod.ETid          
      tmpTT_Anv-Kod.BrukerID      
      tmpTT_Anv-Kod.RegistrertDato
      tmpTT_Anv-Kod.RegistrertTid
      tmpTT_Anv-Kod.RegistrertAv
      .
    FIND TT_Anv-Kod WHERE
        TT_Anv-Kod.Anv-Id = tmpTT_Anv-Kod.Anv-Id NO-ERROR.
    IF NOT AVAILABLE TT_Anv-Kod THEN
        CREATE TT_Anv-Kod.
    BUFFER-COPY tmpTT_Anv-Kod TO TT_Anv-Kod.

    RELEASE TT_Anv-Kod.
    DELETE tmpTT_Anv-Kod.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesArtBestPkt1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesArtBestPkt1.0 Procedure 
PROCEDURE lesArtBestPkt1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

/* TN 4/9-09
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_ArtBestPkt.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_ArtBestPkt.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_ArtBestPkt.ArtikkelNr
      tmpTT_ArtBestPkt.ButikkNr
      tmpTT_ArtBestPkt.StrKode
      tmpTT_ArtBestPkt.EDato
      tmpTT_ArtBestPkt.ETid
      tmpTT_ArtBestPkt.BrukerID
      tmpTT_ArtBestPkt.RegistrertDato
      tmpTT_ArtBestPkt.RegistrertTid
      tmpTT_ArtBestPkt.RegistrertAv.
      .
    FIND TT_ArtBestPkt EXCLUSIVE-LOCK WHERE
      TT_ArtBestPkt.ArtikkelNr = tmpTT_ArtBestPkt.ArtikkelNr AND
      TT_ArtBestPkt.ButikkNr   = tmpTT_ArtBestPkt.ButikkNr AND
      TT_ArtBestPkt.StrKode    = tmpTT_ArtBestPkt.StrKode NO-ERROR.
    /* Skal bare opprettes på artikler som finnes fra før. */
    IF CAN-FIND(ArtBas WHERE
                ArtBas.ArtikkelNr = tmpTT_ArtBestPkt.ArtikkelNr) THEN
    DO:
        IF NOT AVAILABLE TT_ArtBestPkt THEN
            CREATE TT_ArtBestPkt.
        BUFFER-COPY tmpTT_ArtBestPkt TO TT_ArtBestPkt.

    END.
    IF AVAILABLE TT_ArtBestPkt THEN
        RELEASE TT_ArtBestPkt.
    DELETE tmpTT_ArtBestPkt.
  END. /* LOOPEN */

*/ 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesBeliggenhet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesBeliggenhet Procedure 
PROCEDURE lesBeliggenhet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmptt_Beliggenhet.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_Beliggenhet.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_Beliggenhet.BeliggenhetId          
      tmpTT_Beliggenhet.BeliggenhetNavn      
      tmpTT_Beliggenhet.BeliggenhetNotat        
      tmpTT_Beliggenhet.EDato         
      tmpTT_Beliggenhet.ETid          
      tmpTT_Beliggenhet.BrukerID      
      tmpTT_Beliggenhet.RegistrertDato
      tmpTT_Beliggenhet.RegistrertTid 
      tmpTT_Beliggenhet.RegistrertAv
      .
    FIND TT_Beliggenhet WHERE
        TT_Beliggenhet.BeliggenhetId = tmpTT_Beliggenhet.BeliggenhetId NO-ERROR.
    IF NOT AVAILABLE TT_Beliggenhet THEN
        CREATE TT_Beliggenhet.
    BUFFER-COPY tmpTT_Beliggenhet TO TT_Beliggenhet.

    RELEASE TT_Beliggenhet.
    DELETE tmpTT_Beliggenhet.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesDriftsForm1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesDriftsForm1.0 Procedure 
PROCEDURE lesDriftsForm1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmptt_Driftsform.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_Driftsform.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_Driftsform.DriftsFormId          
      tmpTT_Driftsform.DriftsFormNavn      
      tmpTT_Driftsform.DriftsFormNotat        
      tmpTT_Driftsform.EDato         
      tmpTT_Driftsform.ETid          
      tmpTT_Driftsform.BrukerID      
      tmpTT_Driftsform.RegistrertDato
      tmpTT_Driftsform.RegistrertTid 
      tmpTT_Driftsform.RegistrertAv
      .
    FIND TT_Driftsform WHERE
        TT_Driftsform.DriftsFormId = tmpTT_Driftsform.DriftsFormId NO-ERROR.
    IF NOT AVAILABLE TT_Driftsform THEN
        CREATE TT_Driftsform.
    BUFFER-COPY tmpTT_Driftsform TO TT_Driftsform.

    RELEASE TT_Driftsform.
    DELETE tmpTT_Driftsform.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesDriftsType1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesDriftsType1.0 Procedure 
PROCEDURE lesDriftsType1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmptt_DriftsType.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_DriftsType.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_DriftsType.DriftsFormId          
      tmpTT_DriftsType.DriftsTypeId      
      tmpTT_DriftsType.DriftsTypeNavn        
      tmpTT_DriftsType.EDato         
      tmpTT_DriftsType.ETid          
      tmpTT_DriftsType.BrukerID      
      tmpTT_DriftsType.RegistrertDato
      tmpTT_DriftsType.RegistrertTid 
      tmpTT_DriftsType.RegistrertAv
      .
    FIND TT_DriftsType WHERE
        TT_DriftsType.DriftsFormId = tmpTT_DriftsType.DriftsFormId AND
        TT_Driftstype.DriftsTypeId = tmpTT_DriftsType.DriftsTypeId NO-ERROR.
    IF NOT AVAILABLE TT_DriftsType THEN
        CREATE TT_DriftsType.
    BUFFER-COPY tmpTT_DriftsType TO TT_DriftsType.

    RELEASE TT_DriftsType.
    DELETE tmpTT_DriftsType.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesDummy) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesDummy Procedure 
PROCEDURE lesDummy :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop     AS INT NO-UNDO.
  DEF VAR piAntError AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord ON ERROR UNDO, RETRY:
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil
      ^
      NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        piAntError = piAntError + 1.

  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesFarg1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesFarg1.0 Procedure 
PROCEDURE lesFarg1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmptt_Farg.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_Farg.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_Farg.Farg          
      tmpTT_Farg.FarBeskr      
      tmpTT_Farg.KFarge        
      tmpTT_Farg.EDato         
      tmpTT_Farg.ETid          
      tmpTT_Farg.BrukerID      
      tmpTT_Farg.RegistrertDato
      tmpTT_Farg.RegistrertTid 
      tmpTT_Farg.RegistrertAv
      .
    FIND TT_Farg WHERE
        TT_Farg.Farg = tmpTT_Farg.Farg NO-ERROR.
    IF NOT AVAILABLE TT_Farg THEN
        CREATE TT_Farg.
    BUFFER-COPY tmpTT_Farg TO TT_Farg.

    RELEASE TT_Farg.
    DELETE tmpTT_Farg.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesFeilkode1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesFeilkode1.0 Procedure 
PROCEDURE lesFeilkode1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Feilkode.
    ASSIGN
      tmpTT_Feilkode.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_Feilkode.FeilKode      
      tmpTT_Feilkode.Beskrivelse   
      tmpTT_Feilkode.Notat         
      tmpTT_Feilkode.Belastes      
      tmpTT_Feilkode.EDato         
      tmpTT_Feilkode.ETid          
      tmpTT_Feilkode.BrukerID      
      tmpTT_Feilkode.RegistrertDato
      tmpTT_Feilkode.RegistrertTid 
      tmpTT_Feilkode.RegistrertAv
      tmpTT_Feilkode.TransKode
      .
    FIND TT_Feilkode WHERE
        TT_Feilkode.FeilKode = tmpTT_Feilkode.FeilKode NO-ERROR.
    IF NOT AVAILABLE TT_Feilkode THEN
        CREATE TT_Feilkode.
    BUFFER-COPY tmpTT_Feilkode TO TT_Feilkode.

    RELEASE TT_Feilkode.
    DELETE tmpTT_Feilkode.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesFoder1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesFoder1.0 Procedure 
PROCEDURE lesFoder1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Foder.
    ASSIGN
      tmpTT_Foder.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_Foder.foder-id      
      tmpTT_Foder.beskrivning   
      tmpTT_Foder.EDato         
      tmpTT_Foder.ETid          
      tmpTT_Foder.BrukerID      
      tmpTT_Foder.RegistrertDato
      tmpTT_Foder.RegistrertTid 
      tmpTT_Foder.RegistrertAv
      .
    FIND TT_Foder WHERE
        TT_Foder.Foder = tmpTT_Foder.Foder NO-ERROR.
    IF NOT AVAILABLE TT_Foder THEN
        CREATE TT_Foder.
    BUFFER-COPY tmpTT_Foder TO TT_Foder.

    RELEASE TT_Foder.
    DELETE tmpTT_Foder.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesGaranti1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesGaranti1.0 Procedure 
PROCEDURE lesGaranti1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmptt_Garanti.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_Garanti.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_Garanti.garantikl      
      tmpTT_Garanti.garantitekst   
      tmpTT_Garanti.mndant         
      tmpTT_Garanti.bonga5         
      tmpTT_Garanti.fritekst       
      tmpTT_Garanti.EDato         
      tmpTT_Garanti.ETid          
      tmpTT_Garanti.BrukerID      
      tmpTT_Garanti.RegistrertDato
      tmpTT_Garanti.RegistrertTid 
      tmpTT_Garanti.RegistrertAv
      .
    FIND TT_Garanti WHERE
        TT_Garanti.Garantikl = tmpTT_Garanti.garantikl NO-ERROR.
    IF NOT AVAILABLE TT_Garanti THEN
        CREATE TT_Garanti.
    BUFFER-COPY tmpTT_Garanti TO TT_Garanti.

    RELEASE TT_Garanti.
    DELETE tmpTT_Garanti.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesGaveKType1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesGaveKType1.0 Procedure 
PROCEDURE lesGaveKType1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmptt_GaveKType.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_GaveKType.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_GaveKType.IdentType          
      tmpTT_GaveKType.GKTBeskrivelse      
      tmpTT_GaveKType.EDato         
      tmpTT_GaveKType.ETid          
      tmpTT_GaveKType.BrukerID      
      tmpTT_GaveKType.RegistrertDato
      tmpTT_GaveKType.RegistrertTid 
      tmpTT_GaveKType.RegistrertAv
      .
    FIND TT_GaveKType WHERE
        TT_GaveKType.IdentType = tmpTT_GaveKType.IdentType NO-ERROR.
    IF NOT AVAILABLE TT_GaveKType THEN
        CREATE TT_GaveKType.
    BUFFER-COPY tmpTT_GaveKType TO TT_GaveKType.

    RELEASE TT_GaveKType.
    DELETE tmpTT_GaveKType.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesHandtering1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesHandtering1.0 Procedure 
PROCEDURE lesHandtering1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Handtering.
    ASSIGN
      tmpTT_Handtering.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_Handtering.HandKode      
      tmpTT_Handtering.Beskrivelse   
      tmpTT_Handtering.Notat         
      tmpTT_Handtering.EDato         
      tmpTT_Handtering.ETid          
      tmpTT_Handtering.BrukerID      
      tmpTT_Handtering.RegistrertDato
      tmpTT_Handtering.RegistrertTid 
      tmpTT_Handtering.RegistrertAv
      .
    FIND TT_Handtering WHERE
        TT_Handtering.HandKode = tmpTT_Handtering.HandKode NO-ERROR.
    IF NOT AVAILABLE TT_Handtering THEN
        CREATE TT_Handtering.
    BUFFER-COPY tmpTT_Handtering TO TT_Handtering.

    RELEASE TT_Handtering.
    DELETE tmpTT_Handtering.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesHuvGr1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesHuvGr1.0 Procedure 
PROCEDURE lesHuvGr1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_HuvGr.
    ASSIGN
      tmpTT_HuvGr.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_HuvGr.Hg            
      tmpTT_HuvGr.HgBeskr       
      tmpTT_HuvGr.AvdelingNr    
      tmpTT_HuvGr.EDato         
      tmpTT_HuvGr.ETid          
      tmpTT_HuvGr.BrukerID      
      tmpTT_HuvGr.RegistrertDato
      tmpTT_HuvGr.RegistrertTid 
      tmpTT_HuvGr.RegistrertAv
      .
    FIND TT_HuvGr WHERE
        TT_HuvGr.Hg = tmpTT_HuvGr.Hg NO-ERROR.
    IF NOT AVAILABLE TT_HuvGr THEN
        CREATE TT_HuvGr.
    BUFFER-COPY tmpTT_HuvGr TO TT_HuvGr.

    RELEASE TT_HuvGr.
    DELETE tmpTT_HuvGr.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesIndType1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesIndType1.0 Procedure 
PROCEDURE lesIndType1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmptt_Indtype.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_Indtype.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_Indtype.IndividType   
      tmpTT_Indtype.IndividBeskr  
      tmpTT_Indtype.SerieNrReg    
      tmpTT_Indtype.EDato         
      tmpTT_Indtype.ETid          
      tmpTT_Indtype.BrukerID      
      tmpTT_Indtype.RegistrertDato
      tmpTT_Indtype.RegistrertTid 
      tmpTT_Indtype.RegistrertAv
      .
    FIND TT_Indtype WHERE
        TT_Indtype.IndividType = tmpTT_Indtype.IndividType NO-ERROR.
    IF NOT AVAILABLE TT_Indtype THEN
        CREATE TT_Indtype.
    BUFFER-COPY tmpTT_Indtype TO TT_Indtype.

    RELEASE TT_Indtype.
    DELETE tmpTT_Indtype.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesInnBetType1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesInnBetType1.0 Procedure 
PROCEDURE lesInnBetType1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmptt_InnBetType.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_InnBetType.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_InnBetType.InnBetTId          
      tmpTT_InnBetType.InnBBeskrivelse    
      tmpTT_InnBetType.KontoNr            
      tmpTT_InnBetType.EDato         
      tmpTT_InnBetType.ETid          
      tmpTT_InnBetType.BrukerID      
      tmpTT_InnBetType.RegistrertDato
      tmpTT_InnBetType.RegistrertTid 
      tmpTT_InnBetType.RegistrertAv
      .
    FIND TT_InnBetType WHERE
        TT_InnBetType.InnBetTId = tmpTT_InnBetType.InnBetTId NO-ERROR.
    IF NOT AVAILABLE TT_InnBetType THEN
        CREATE TT_InnBetType.
    BUFFER-COPY tmpTT_InnBetType TO TT_InnBetType.

    RELEASE TT_InnBetType.
    DELETE tmpTT_InnBetType.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesInnersula1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesInnersula1.0 Procedure 
PROCEDURE lesInnersula1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Innersula.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_Innersula.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_Innersula.Inner-Id      
      tmpTT_Innersula.InnerBeskr    
      tmpTT_Innersula.EDato         
      tmpTT_Innersula.ETid          
      tmpTT_Innersula.BrukerID      
      tmpTT_Innersula.RegistrertDato
      tmpTT_Innersula.RegistrertTid 
      tmpTT_Innersula.RegistrertAv  
     .
    FIND TT_Innersula WHERE
        TT_Innersula.Inner-Id = tmpTT_Innersula.Inner-Id NO-ERROR.
    IF NOT AVAILABLE TT_Innersula THEN
        CREATE TT_Innersula.
    BUFFER-COPY tmpTT_Innersula TO TT_Innersula.

    RELEASE TT_Innersula.
    DELETE tmpTT_Innersula.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LesInnFil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LesInnFil Procedure 
PROCEDURE LesInnFil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR piLinjeNr AS INT  NO-UNDO.
  DEF VAR pcLinje   AS CHAR NO-UNDO.
  DEF VAR piLoop    AS INT  NO-UNDO.
  DEF VAR pbOk      AS LOG  NO-UNDO.
  DEF VAR piAntFeil AS INT  NO-UNDO.
  DEF VAR piLesteLinjer AS INT NO-UNDO.

  /* Tømmer feillogg. */
  FOR EACH tt_Error:
    DELETE tt_Error.
  END.

  RUN TellOppLinjer.

  ASSIGN
    cTekst = "Antall linjer i filen " + STRING(iTotAntLinjer) + ".".
  PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

  FIND LAST VPIFilLinje OF VPIFilHode NO-LOCK NO-ERROR.
  IF AVAILABLE VPIFilLinje THEN
      piLinjeNr = VPIFilLinje.LinjeNr + 1.
  ELSE
      piLinjeNr = 1.

  ASSIGN
      pbOk        = TRUE
      cType       = ""
      iAntLinjer   = 0
      piAntFeil   = 0
      iRecType    = 0  
      cTabellNavn = ""   
      cRecVersion = ""  
      iAntRecord  = 0  
      cTblLst     = ""
      piLoop      = 0
      .

  INPUT STREAM InnFil FROM VALUE(cFilNavn) NO-ECHO.
  LESERLINJER:
  REPEAT:
    ASSIGN
      piLesteLinjer = piLesteLinjer + 1.
      iAntLinjer    = iAntLinjer    + 1
      .
    /* Leser linje fra filen */
    IMPORT STREAM InnFil
      cType
      cTabellNavn
      iRecType   
      cRecVersion
      iAntRecord 
      NO-ERROR.
    /* Feil på linje */
    IF ERROR-STATUS:ERROR = TRUE THEN
    DO:
      ASSIGN
        piAntFeil = piAntFeil + 1
        .
      CREATE tt_Error.
      ASSIGN
        tt_Error.LinjeNr = piAntFeil
        tt_Error.Tekst   = "** Feil på linje " + STRING(iAntLinjer) + "."
        .
      NEXT LESERLINJER.
    END.

    /* Sjekker at filen er ok */
    IF cType <> "H" THEN
    DO:
      ASSIGN
        pbOk = FALSE
        cTekst = "** Ødelagt fil - eller data for ukjent tabell (" + cTabellNavn + ") i filen. Innlesning avbrytes. ".
      PUBLISH "VPIFilLogg" (cTekst + chr(1) + "40").
      LEAVE LESERLINJER.
    END.
    /* Rapporterer mottatte fildata */
    ASSIGN
      lTid = TIME
      cTekst = "Linje : " + string(iAntLinjer) + " Tabell: " + cTabellNavn + (IF iRecType = 3
                                           THEN " Slettet"
                                           ELSE " Ny/Endret") + " - antall " + STRING(iAntRecord) + ".".
    PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").

    /* Logger hvilke tabeller som er kommet. */
    IF NOT CAN-DO(cTblLst,cTabellNavn) THEN
      cTblLst = cTblLst + (IF cTblLst = "" THEN "" ELSE ",") + cTabellNavn.

    /* Bygger temp-Tabeller */
    CASE (cTabellNavn + cRecVersion):
      WHEN "Avdeling1.0"        THEN RUN value("lesAvdeling"        + cRecVersion + '.p') (iRecType,iAntRecord,OUTPUT iAntLinjer). 
      WHEN "DefaultLevDato1.0"  THEN RUN value("lesDefaultLevDato"  + cRecVersion + '.p') (iRecType,iAntRecord,OUTPUT iAntLinjer). 
      WHEN "VarGr1.0"           THEN RUN value("lesVarGr"           + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Kategori1.0"        THEN RUN value("lesKategori"        + cRecVersion) (iRecType,iAntRecord). 
      WHEN "HuvGr1.0"           THEN RUN value("lesHuvGr"           + cRecVersion) (iRecType,iAntRecord). 
      WHEN "LevBas1.0"          THEN RUN value("lesLevBas"          + cRecVersion + '.p') (iRecType,iAntRecord,OUTPUT iAntLinjer). 
      WHEN "VgKat1.0"           THEN RUN value("lesVgKat"           + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Produsent1.0"       THEN RUN value("lesProdusent"       + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Varemerke1.0"       THEN RUN value("lesVaremerke"       + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Sasong1.0"          THEN RUN value("lesSasong"          + cRecVersion) (iRecType,iAntRecord). 
      WHEN "StrKonv1.0"         THEN RUN value("lesStrKonv"         + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Farg1.0"            THEN RUN value("lesFarg"            + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Material1.0"        THEN RUN value("lesMaterial"        + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Klack1.0"           THEN RUN value("lesKlack"           + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Innersula1.0"       THEN RUN value("lesInnersula"       + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Ovandel1.0"         THEN RUN value("lesOvandel"         + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Foder1.0"           THEN RUN value("lesFoder"           + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Slitsula1.0"        THEN RUN value("lesSlitsula"        + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Last-Sko1.0"        THEN RUN value("lesLast-Sko"        + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Anv-Kod1.0"         THEN RUN value("lesAnv-Kod"         + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Behandlingskode1.0" THEN RUN value("lesBehandlingskode" + cRecVersion + '.p') (iRecType,iAntRecord,OUTPUT iAntLinjer). 
      WHEN "Feilkode1.0"        THEN RUN value("lesFeilkode"        + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Handtering1.0"      THEN RUN value("lesHandtering"      + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Kravkode1.0"        THEN RUN value("lesKravkode"        + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Prisgruppe1.0"      THEN RUN value("lesPrisgruppe"      + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Prisprofil1.0"      THEN RUN value("lesPrisprofil"      + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Prov1.0"            THEN RUN value("lesProv"            + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Rabatt1.0"          THEN RUN value("lesRabatt"          + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Moms1.0"            THEN RUN value("lesMoms"            + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Post1.0"            THEN RUN value("lesPost"            + cRecVersion) (iRecType,iAntRecord). 
      WHEN "StrType1.0"         THEN RUN value("lesStrType"         + cRecVersion) (iRecType,iAntRecord). 
      WHEN "StrTstr1.0"         THEN RUN value("lesStrTstr"         + cRecVersion) (iRecType,iAntRecord). 
      WHEN "LevSort1.0"         THEN RUN value("lesLevSort"         + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Kjede1.0"           THEN RUN value("lesKjede"           + cRecVersion) (iRecType,iAntRecord). 
      WHEN "KjedeDistrikt1.0"   THEN RUN value("lesKjedeDistrikt"   + cRecVersion) (iRecType,iAntRecord). 
      WHEN "KjedensButikker1.0" THEN RUN value("lesKjedensButikker" + cRecVersion) (iRecType,iAntRecord). 
      WHEN "KjedeRegion1.0"     THEN RUN value("lesKjedeRegion"     + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Valuta1.0"          THEN RUN value("lesValuta"          + cRecVersion) (iRecType,iAntRecord). 
      WHEN "KasValuta1.0"       THEN RUN value("lesKasValuta"       + cRecVersion) (iRecType,iAntRecord). 
      WHEN "VPIArtBas1.0"       THEN RUN value("lesVPIArtBas"       + cRecVersion + '.p') (iRecType,iAntRecord,OUTPUT iAntLinjer). 
      WHEN "VPIArtPris1.0"      THEN RUN value("lesVPIArtPris"      + cRecVersion + '.p') (iRecType,iAntRecord,OUTPUT iAntLinjer). 
      WHEN "VPIStrekkode1.0"    THEN RUN value("lesVPIStrekkode"    + cRecVersion) (iRecType,iAntRecord). 
      WHEN "VPIBildeData1.0"    THEN RUN value("lesVPIBildeData"    + cRecVersion) (iRecType,iAntRecord). 
      WHEN "VPIBildeRegister1.0" THEN RUN value("lesVPIBildeRegister"    + cRecVersion) (iRecType,iAntRecord). 
      WHEN "VPIErstattningsvare1.0" THEN RUN value("lesVPIErstattningsvare"    + cRecVersion) (iRecType,iAntRecord). 
      WHEN "VPIPakkeLinje1.0"   THEN RUN value("lesVPIPakkeLinje"   + cRecVersion) (iRecType,iAntRecord). 
      WHEN "ProgramListe1.0"    THEN RUN value("lesProgramListe"    + cRecVersion) (iRecType,iAntRecord). 
      WHEN "UtbetType1.0"       THEN RUN value("lesUtbetType"       + cRecVersion) (iRecType,iAntRecord). 
      WHEN "VareBehType1.0"     THEN RUN value("lesVareBehType"     + cRecVersion) (iRecType,iAntRecord). 
      WHEN "VareBoktype1.0"     THEN RUN value("lesVareBoktype"     + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Messe1.0"           THEN RUN value("lesMesse"           + cRecVersion) (iRecType,iAntRecord). 
      WHEN "LokalGruppering1.0" THEN RUN value("lesLokalGruppering" + cRecVersion) (iRecType,iAntRecord). 
      WHEN "IndType1.0"         THEN RUN value("lesIndType"         + cRecVersion) (iRecType,iAntRecord). 
      WHEN "InnbetType1.0"      THEN RUN value("lesInnbetType"      + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Garanti1.0"         THEN RUN value("lesGaranti"         + cRecVersion) (iRecType,iAntRecord). 
      WHEN "GaveKType1.0"       THEN RUN value("lesGaveKType"       + cRecVersion) (iRecType,iAntRecord). 
      WHEN "DriftsForm1.0"      THEN RUN value("lesDriftsForm"      + cRecVersion) (iRecType,iAntRecord). 
      WHEN "DriftsType1.0"      THEN RUN value("lesDriftsType"      + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Aktivitet1.0"       THEN RUN value("lesAktivitet"       + cRecVersion) (iRecType,iAntRecord). 
      WHEN "Beliggenhet1.0"     THEN RUN value("lesBeliggenhet"     + cRecVersion) (iRecType,iAntRecord). 
      WHEN "VgAkt1.0"           THEN RUN value("lesVgAkt"           + cRecVersion) (iRecType,iAntRecord). 
      WHEN "AltLevBas1.0"       THEN RUN value("lesAltLevBas"       + cRecVersion) (iRecType,iAntRecord).
      /* TN 4/9-09 Kobler ut disse da de ikke er nødvendige - Programmet sprekker i kompilering. 
      WHEN "ArtBestPkt1.0"      THEN RUN value("lesArtBestPkt"      + cRecVersion) (iRecType,iAntRecord). 
      WHEN "LevKontakt1.0"      THEN RUN value("lesLevKontakt"      + cRecVersion) (iRecType,iAntRecord).
      */ 
      WHEN "EkstVPILev1.0"      THEN DO:
          RUN value("lesEkstVPILev"      + cRecVersion + '.p') (iRecType,iAntRecord,OUTPUT iAntLinjer).  /* Eksternt program */
      END.
      OTHERWISE DO:
        ASSIGN
          pbOk   = FALSE
          cTekst = "** Feil versjon tabell " + cTabellnavn + ". Versjon : " + cRecVersion + 
                   " (Antall linjer " + STRING(iAntRecord) + ").".
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "40").
        RUN lesDummy (iRecType,iAntRecord).
      END.
    END CASE.
    STATUS DEFAULT "Leser linje " + 
                   STRING(iAntLinjer) + 
                   " av " + 
                   STRING(iTotAntLinjer) + 
                   ".".
    ASSIGN lTid = TIME - lTid.
    if bLogg then RUN bibl_logg.p ('xhkvpiinnles', 'xhkvpiinnles.p: Innlesning av ' + cTabellNavn + cRecVersion + ' ' + string(TIME,"HH:MM:SS") + ' Tidsbruk: ' + string(lTid,"HH:MM:SS")).

  END. /* LESERLINJER */
  INPUT STREAM InnFil CLOSE.

  STATUS DEFAULT "Oppdaterer databasen med innleste data.".

  /* Behandler de innleste grunnregistrene */
  DO piLoop = 1 TO NUM-ENTRIES(cTblLst):
      ASSIGN lTid = TIME.
      if bLogg then RUN bibl_logg.p ('xhkvpiinnles', 'xhkvpiinnles.p: START Oppdatering av ' + ENTRY(piLoop,cTblLst) + ' ' + string(TIME,"HH:MM:SS")).
    CASE ENTRY(piLoop,cTblLst):
      WHEN "Avdeling"        THEN RUN oppdAvdeling.
      WHEN "VarGr"           THEN RUN oppdVarGr.
      WHEN "Kategori"        THEN RUN oppdKategori.
      WHEN "HuvGr"           THEN RUN oppdHuvGr.
      WHEN "LevBas"          THEN RUN oppdLevBas.p. /* Ekstern rutine */
      WHEN "DefaultLevDato"  THEN RUN oppdDefaultLevDato.p. /* Ekstern rutine */
      WHEN "VgKat"           THEN RUN oppdVgKat.
      WHEN "Produsent"       THEN RUN oppdProdusent.
      WHEN "Varemerke"       THEN RUN oppdVaremerke.
      WHEN "Sasong"          THEN RUN oppdSasong.
      WHEN "StrKonv"         THEN RUN oppdStrKonv.
      WHEN "Farg"            THEN RUN oppdFarg.
      WHEN "Material"        THEN RUN oppdMaterial.
      WHEN "Klack"           THEN RUN oppdKlack.
      WHEN "Innersula"       THEN RUN oppdInnersula.
      WHEN "Ovandel"         THEN RUN oppdOvandel.
      WHEN "Foder"           THEN RUN oppdFoder.
      WHEN "Slitsula"        THEN RUN oppdSlitsula.
      WHEN "Last-Sko"        THEN RUN oppdLast-Sko.
      WHEN "Anv-Kod"         THEN RUN oppdAnv-Kod.
      WHEN "Behandlingskode" THEN RUN oppdBehandlingskode.
      WHEN "Feilkode"        THEN RUN oppdFeilkode.
      WHEN "Handtering"      THEN RUN oppdHandtering.
      WHEN "Kravkode"        THEN RUN oppdKravkode.
      WHEN "Prisgruppe"      THEN RUN oppdPrisgruppe.
      WHEN "Prisprofil"      THEN RUN oppdPrisprofil.
      WHEN "Prov"            THEN RUN oppdProv.
      WHEN "Rabatt"          THEN RUN oppdRabatt.
      WHEN "Moms"            THEN RUN oppdMoms.
      WHEN "Post"            THEN RUN oppdPost.
      WHEN "StrType"         THEN RUN oppdStrType.
      WHEN "LevSort"         THEN RUN oppdLevSort.
      WHEN "Kjede"           THEN RUN oppdKjede.
      WHEN "KjedeDistrikt"   THEN RUN oppdKjedeDistrikt.
      WHEN "KjedensButikker" THEN RUN oppdKjedensButikker.
      WHEN "KjedeRegion"     THEN RUN oppdKjedeRegion.
      WHEN "Valuta"          THEN RUN oppdValuta.
      WHEN "KasValuta"       THEN RUN oppdKasValuta.
      WHEN "VPIArtBas"       THEN RUN oppdVPIArtBas.p.  /* ekstern rutine. */
      WHEN "VPIStrekkode"    THEN RUN oppdVPIStrekkode.
      WHEN "VPIArtPris"      THEN RUN oppdVPIArtPris.p. /* ekstern rutine. */
      WHEN "VPIBildeData"    THEN RUN oppdVPIBildeData.
      WHEN "VPIBildeRegister" THEN RUN oppdVPIBildeRegister.
      WHEN "VPIErstattningsvare" THEN RUN oppdVPIErstattningsvare.
      WHEN "VPIPakkeLinje"   THEN RUN oppdVPIPakkeLinje.
      WHEN "ProgramListe"    THEN RUN oppdProgramListe.
      WHEN "UtbetType"       THEN RUN oppdUtbetType.
      WHEN "VareBehType"     THEN RUN oppdVareBehType. 
      WHEN "VareBoktype"     THEN RUN oppdVareBoktype. 
      WHEN "Messe"           THEN RUN oppdMesse. 
      WHEN "LokalGruppering" THEN RUN oppdLokalGruppering. 
      WHEN "IndType"         THEN RUN oppdIndType. 
      WHEN "InnbetType"      THEN RUN oppdInnbetType. 
      WHEN "Garanti"         THEN RUN oppdGaranti. 
      WHEN "GaveKType"       THEN RUN oppdGaveKType. 
      WHEN "DriftsForm"      THEN RUN oppdDriftsForm. 
      WHEN "DriftsType"      THEN RUN oppdDriftsType. 
      WHEN "Aktivitet"       THEN RUN oppdAktivitet. 
      WHEN "Beliggenhet"     THEN RUN oppdBeliggenhet. 
      WHEN "VgAkt"           THEN RUN oppdVgAkt. 
      WHEN "AltLevBas"       THEN RUN oppdAltLevBas. 
      WHEN "ArtBestPkt"      THEN RUN oppdArtBestPkt. 
      WHEN "LevKontakt"      THEN RUN oppdLevKontakt. 
      WHEN "EkstVPILev"      THEN RUN oppdEkstVPILev.p. /* ekstern rutine. */
      OTHERWISE. /* Gjør ingenting. */
    END CASE.
    ASSIGN lTid = TIME - lTid.
    if bLogg then RUN bibl_logg.p ('xhkvpiinnles', 'xhkvpiinnles.p: SLUTT Oppdatering av ' + ENTRY(piLoop,cTblLst) + ' ' + string(TIME,"HH:MM:SS") + ' Tidsbruk: ' + string(lTid,"HH:MM:SS")).
  END.

  /* Stempler posten som innlest og oppdatert. */
  DO TRANSACTION:
      FIND CURRENT VPIFilHode EXCLUSIVE-LOCK.
      ASSIGN
          VPIFilHode.VPIFilStatus = 5
          .
      IF pbOk = FALSE THEN
      DO:
        ASSIGN
          cTekst = "** Feil ved oppdatering av fil." 
          .
        PUBLISH "VPIFilLogg" (cTekst + chr(1) + "40").
      END.
      ASSIGN
        cTekst = "Fil oppdatert (Antall linjer: " + STRING(iAntLinjer) + ").". 
        .
      PUBLISH "VPIFilLogg" (cTekst + chr(1) + "5").
  END.
  IF AVAILABLE VPIFilHode THEN
      FIND CURRENT VPIFilHode    NO-LOCK.

  IF CAN-FIND(FIRST tt_Error) THEN
    RUN ErrorLogg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesKasValuta1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesKasValuta1.0 Procedure 
PROCEDURE lesKasValuta1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_KasValuta.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_KasValuta.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_KasValuta.ValKod        
      tmpTT_KasValuta.ValKurs       
      tmpTT_KasValuta.ValLand       
      tmpTT_KasValuta.ValDatum      
      tmpTT_KasValuta.EDato         
      tmpTT_KasValuta.ETid          
      tmpTT_KasValuta.BrukerID      
      tmpTT_KasValuta.RegistrertDato
      tmpTT_KasValuta.RegistrertTid 
      tmpTT_KasValuta.RegistrertAv  
      tmpTT_KasValuta.ValNr         
      tmpTT_KasValuta.ValNavn       
      tmpTT_KasValuta.indeks        
      tmpTT_KasValuta.retur         
      tmpTT_KasValuta.KasseValKurs  
      tmpTT_KasValuta.ValAktiv.
      tmpTT_KasValuta.EgenValuta.
      .

      FIND TT_KasValuta WHERE
          TT_KasValuta.ValKod = tmpTT_KasValuta.ValKod NO-ERROR.
      IF NOT AVAILABLE TT_KasValuta THEN
          CREATE TT_KasValuta.
      BUFFER-COPY tmpTT_KasValuta TO TT_KasValuta.

      RELEASE TT_KasValuta.
      DELETE tmpTT_KasValuta.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesKategori1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesKategori1.0 Procedure 
PROCEDURE lesKategori1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Kategori.
    ASSIGN
      tmpTT_Kategori.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_Kategori.KatNr         
      tmpTT_Kategori.Beskrivelse   
      tmpTT_Kategori.Merknad       
      tmpTT_Kategori.EDato         
      tmpTT_Kategori.ETid          
      tmpTT_Kategori.BrukerID      
      tmpTT_Kategori.RegistrertDato
      tmpTT_Kategori.RegistrertTid 
      tmpTT_Kategori.RegistrertAv
      .
    FIND TT_Kategori WHERE
        TT_Kategori.KatNr = tmpTT_Kategori.KatNr NO-ERROR.
    IF NOT AVAILABLE TT_Kategori THEN
        CREATE TT_Kategori.
    BUFFER-COPY tmpTT_Kategori TO TT_Kategori.

    RELEASE TT_Kategori.
    DELETE tmpTT_Kategori.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesKjede1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesKjede1.0 Procedure 
PROCEDURE lesKjede1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Kjede.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_Kjede.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_Kjede.KjedeNr       
      tmpTT_Kjede.KjedeNavn     
      tmpTT_Kjede.KjedeKNavn    
      tmpTT_Kjede.EDato         
      tmpTT_Kjede.ETid          
      tmpTT_Kjede.BrukerID      
      tmpTT_Kjede.RegistrertDato
      tmpTT_Kjede.RegistrertTid 
      tmpTT_Kjede.RegistrertAv.
      .
      FIND TT_Kjede WHERE
          TT_Kjede.KjedeNr = tmpTT_Kjede.KjedeNr NO-ERROR.
      IF NOT AVAILABLE TT_Kjede THEN
          CREATE TT_Kjede.
      BUFFER-COPY tmpTT_Kjede TO TT_Kjede.

      RELEASE TT_Kjede.
      DELETE tmpTT_Kjede.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesKjedeDistrikt1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesKjedeDistrikt1.0 Procedure 
PROCEDURE lesKjedeDistrikt1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_KjedeDistrikt.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_KjedeDistrikt.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_KjedeDistrikt.KjedeNr       
      tmpTT_KjedeDistrikt.EDato         
      tmpTT_KjedeDistrikt.ETid          
      tmpTT_KjedeDistrikt.BrukerID      
      tmpTT_KjedeDistrikt.RegistrertDato
      tmpTT_KjedeDistrikt.RegistrertTid 
      tmpTT_KjedeDistrikt.RegistrertAv  
      tmpTT_KjedeDistrikt.RegionNr      
      tmpTT_KjedeDistrikt.DistriktNavn  
      tmpTT_KjedeDistrikt.DistriktKNavn 
      tmpTT_KjedeDistrikt.Kontaktperson 
      tmpTT_KjedeDistrikt.Telefon       
      tmpTT_KjedeDistrikt.Mombil        
      tmpTT_KjedeDistrikt.EMail         
      tmpTT_KjedeDistrikt.DistriktNr
      .
    FIND TT_KjedeDistrikt WHERE
        TT_KjedeDistrikt.KjedeNr    = tmpTT_KjedeDistrikt.KjedeNr AND 
        TT_KjedeDistrikt.RegionNr   = tmpTT_KjedeDistrikt.RegionNr AND
        TT_KjedeDistrikt.DistriktNr = tmpTT_KjedeDistrikt.DistriktNr 
        NO-ERROR.
    IF NOT AVAILABLE TT_KjedeDistrikt THEN
        CREATE TT_KjedeDistrikt.
    BUFFER-COPY tmpTT_KjedeDistrikt TO TT_KjedeDistrikt.

    RELEASE TT_KjedeDistrikt.
    DELETE tmpTT_KjedeDistrikt.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesKjedensButikker1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesKjedensButikker1.0 Procedure 
PROCEDURE lesKjedensButikker1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_KjedensButikker.
    ASSIGN
      tmpTT_KjedensButikker.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_KjedensButikker.ButikkNr      
      tmpTT_KjedensButikker.ButikkNavn    
      tmpTT_KjedensButikker.Kontaktperson 
      tmpTT_KjedensButikker.E-Mail        
      tmpTT_KjedensButikker.Telefon       
      tmpTT_KjedensButikker.Mobil         
      tmpTT_KjedensButikker.Telefaks      
      tmpTT_KjedensButikker.PostNr        
      tmpTT_KjedensButikker.Firmanavn     
      tmpTT_KjedensButikker.DagligLeder   
      tmpTT_KjedensButikker.Adresse1      
      tmpTT_KjedensButikker.Adresse2      
      tmpTT_KjedensButikker.Medlemsstatus 
      tmpTT_KjedensButikker.KjedeNr       
      tmpTT_KjedensButikker.RegionNr      
      tmpTT_KjedensButikker.DistriktNr    
      tmpTT_KjedensButikker.EDato         
      tmpTT_KjedensButikker.ETid          
      tmpTT_KjedensButikker.BrukerID      
      tmpTT_KjedensButikker.RegistrertDato
      tmpTT_KjedensButikker.RegistrertTid 
      tmpTT_KjedensButikker.RegistrertAv
      tmpTT_KjedensButikker.OppstartButikkdata 
      tmpTT_KjedensButikker.DriftsFormId       
      tmpTT_KjedensButikker.DriftsTypeId       
      tmpTT_KjedensButikker.BeliggenhetId      
      tmpTT_KjedensButikker.UtmeldtDato        
      tmpTT_KjedensButikker.LGId               
      .
    FIND TT_KjedensButikker WHERE
        TT_KjedensButikker.ButikkNr = tmpTT_KjedensButikker.ButikkNr 
        NO-ERROR.
    IF NOT AVAILABLE TT_KjedensButikker THEN
        CREATE TT_KjedensButikker.
    BUFFER-COPY tmpTT_KjedensButikker TO TT_KjedensButikker.

    RELEASE TT_KjedensButikker.
    DELETE tmpTT_KjedensButikker.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesKjedeRegion1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesKjedeRegion1.0 Procedure 
PROCEDURE lesKjedeRegion1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_KjedeRegion.
    ASSIGN
      tmpTT_KjedeRegion.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_KjedeRegion.KjedeNr       
      tmpTT_KjedeRegion.EDato         
      tmpTT_KjedeRegion.ETid          
      tmpTT_KjedeRegion.BrukerID      
      tmpTT_KjedeRegion.RegistrertDato
      tmpTT_KjedeRegion.RegistrertTid 
      tmpTT_KjedeRegion.RegistrertAv  
      tmpTT_KjedeRegion.RegionNr      
      tmpTT_KjedeRegion.RegionNavn    
      tmpTT_KjedeRegion.RegionKNavn   
      tmpTT_KjedeRegion.Kontaktperson 
      tmpTT_KjedeRegion.Telefon       
      tmpTT_KjedeRegion.Mombil        
      tmpTT_KjedeRegion.EMail
      .
    FIND TT_KjedeRegion WHERE
        TT_KjedeRegion.KjedeNr  = tmpTT_KjedeRegion.KjedeNr AND 
        TT_KjedeRegion.RegionNr = tmpTT_KjedeRegion.RegionNr 
        NO-ERROR.
    IF NOT AVAILABLE TT_KjedeRegion THEN
        CREATE TT_KjedeRegion.
    BUFFER-COPY tmpTT_KjedeRegion TO TT_KjedeRegion.

    RELEASE TT_KjedeRegion.
    DELETE tmpTT_KjedeRegion.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesKlack1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesKlack1.0 Procedure 
PROCEDURE lesKlack1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Klack.
    ASSIGN
      tmpTT_Klack.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_Klack.beskrivning   
      tmpTT_Klack.klack-id      
      tmpTT_Klack.EDato         
      tmpTT_Klack.ETid          
      tmpTT_Klack.BrukerID      
      tmpTT_Klack.RegistrertDato
      tmpTT_Klack.RegistrertTid 
      tmpTT_Klack.RegistrertAv.
      .
      FIND TT_Klack WHERE
          TT_Klack.Klack-Id = tmpTT_Klack.Klack-Id 
          NO-ERROR.
      IF NOT AVAILABLE TT_Klack THEN
          CREATE TT_Klack.
      BUFFER-COPY tmpTT_Klack TO TT_Klack.

      RELEASE TT_Klack.
      DELETE tmpTT_Klack.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesKravKode1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesKravKode1.0 Procedure 
PROCEDURE lesKravKode1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Kravkode.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_Kravkode.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_Kravkode.KravKode      
      tmpTT_Kravkode.Beskrivelse   
      tmpTT_Kravkode.Notat         
      tmpTT_Kravkode.EDato         
      tmpTT_Kravkode.ETid          
      tmpTT_Kravkode.BrukerID      
      tmpTT_Kravkode.RegistrertDato
      tmpTT_Kravkode.RegistrertTid 
      tmpTT_Kravkode.RegistrertAv
      tmpTT_Kravkode.TransKode
      .
    FIND TT_Kravkode WHERE
        TT_Kravkode.KravKode = tmpTT_Kravkode.KravKode 
        NO-ERROR.
    IF NOT AVAILABLE TT_Kravkode THEN
        CREATE TT_Kravkode.
    BUFFER-COPY tmpTT_Kravkode TO TT_Kravkode.

    RELEASE TT_Kravkode.
    DELETE tmpTT_Kravkode.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesLast-Sko1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesLast-Sko1.0 Procedure 
PROCEDURE lesLast-Sko1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Last-Sko.
    ASSIGN
      tmpTT_Last-Sko.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_Last-Sko.Last-Id       
      tmpTT_Last-Sko.LastBeskr     
      tmpTT_Last-Sko.EDato         
      tmpTT_Last-Sko.ETid          
      tmpTT_Last-Sko.BrukerID      
      tmpTT_Last-Sko.RegistrertDato
      tmpTT_Last-Sko.RegistrertTid 
      tmpTT_Last-Sko.RegistrertAv
      .
    FIND Last-Sko WHERE
        Last-Sko.Last-Id = tmpTT_Last-Sko.Last-Id 
        NO-ERROR.
    IF NOT AVAILABLE TT_Last-Sko THEN
        CREATE TT_Last-Sko.
    BUFFER-COPY tmpTT_Last-Sko TO TT_Last-Sko.

    RELEASE TT_Last-Sko.
    DELETE tmpTT_Last-Sko.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesLevKontakt1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesLevKontakt1.0 Procedure 
PROCEDURE lesLevKontakt1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

/* TN 4/9-09
  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmptt_Levkontakt.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_LevKontakt.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_LevKontakt.LevNr          
      tmpTT_LevKontakt.Navn           
      tmpTT_LevKontakt.Stilling       
      tmpTT_LevKontakt.Telefon        
      tmpTT_LevKontakt.Telefaks       
      tmpTT_LevKontakt.Mobiltelefon   
      tmpTT_LevKontakt.Merknad        
      tmpTT_LevKontakt.EDato          
      tmpTT_LevKontakt.ETid           
      tmpTT_LevKontakt.BrukerID       
      tmpTT_LevKontakt.RegistrertDato 
      tmpTT_LevKontakt.RegistrertTid  
      tmpTT_LevKontakt.RegistrertAv   
      tmpTT_LevKontakt.KontNr   
      tmpTT_LevKontakt.E_MailKontakt
      .
    FIND TT_LevKontakt WHERE
        TT_LevKontakt.LevNr  = tmpTT_LevKontakt.LevNr AND
        TT_LEvKontakt.KontNr = tmpTT_Levkontakt.KontNr NO-ERROR.
    IF NOT AVAILABLE TT_LevKontakt THEN
        CREATE TT_LevKontakt.
    BUFFER-COPY tmpTT_LevKontakt TO TT_LevKontakt.

    RELEASE TT_LevKontakt.
    DELETE tmpTT_LevKontakt.
  END. /* LOOPEN */

*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesLevSort1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesLevSort1.0 Procedure 
PROCEDURE lesLevSort1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop         AS INT  NO-UNDO.
  DEF VAR piLoop2        AS INT  NO-UNDO.
  DEF VAR pcType         AS CHAR NO-UNDO.
  DEF VAR pi2RecType      AS INT  NO-UNDO. 
  DEF VAR pcTabellNavn   AS CHAR NO-UNDO. 
  DEF VAR pcRecVersion   AS CHAR NO-UNDO.
  DEF VAR pi2AntRecord    AS INT  NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_LevSort.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_LevSort.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_LevSort.LevNr      
      tmpTT_LevSort.StrTypeID  
      tmpTT_LevSort.SortID     
      tmpTT_LevSort.Beskrivelse
      tmpTT_LevSort.Merknad    
      tmpTT_LevSort.Fri
      NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
    DO:
        IF AVAILABLE tmpTT_LevSort THEN
            DELETE tmpTT_LevSort.
        NEXT LOOPEN.
    END.

    /* Leser Header record for StrTStr postene */
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      .
    /* Leser linje fra filen */
    IMPORT STREAM InnFil
      pcType
      pcTabellNavn
      pi2RecType   
      pcRecVersion
      pi2AntRecord 
      NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT LOOPEN.
    
    LOOPEN2:
    DO piLoop2 = 1 TO pi2AntRecord:
      CREATE tmpTT_LevSAnt.
      ASSIGN
        iAntLinjer = iAntLinjer + 1
        tmpTT_LevSAnt.RecType = pi2RecType
        .
      IMPORT STREAM InnFil 
        tmpTT_LevSAnt.SoStorl
        tmpTT_LevSAnt.SoAnt  
        tmpTT_LevSAnt.SeqNr  
        tmpTT_LevSAnt.LevNr  
        tmpTT_LevSAnt.SortID
        NO-ERROR.
      IF ERROR-STATUS:ERROR THEN
      DO:
          IF AVAILABLE tmpTT_LevSAnt THEN
              DELETE tmpTT_LevSAnt.
          NEXT LOOPEN2.
      END.

      FIND TT_LevSAnt WHERE 
          TT_LevSAnt.LevNr  = tmpTT_LevSAnt.LevNr AND
          TT_LevSAnt.SortId = tmpTT_LevSAnt.SortId AND
          TT_LevSAnt.SeqNr  = tmpTT_LevSAnt.SeqNr
          NO-ERROR.
      IF NOT AVAILABLE TT_LevSAnt THEN
          CREATE TT_LevSAnt.
      BUFFER-COPY tmpTT_LevSAnt TO TT_LevSAnt.

      RELEASE TT_LevSAnt.
      DELETE tmpTT_LevSAnt.
    END. /* LOOPEN2 */

    IF AVAILABLE tmpTT_LevSAnt THEN
      RELEASE tmpTT_LevSAnt.

    FIND TT_LevSort WHERE 
        TT_LevSort.LevNr  = tmpTT_LevSort.LevNr AND
        TT_LevSort.SortId = tmpTT_LevSort.SortId
        NO-ERROR.
    IF NOT AVAILABLE TT_LevSort THEN
        CREATE TT_LevSort.
    BUFFER-COPY tmpTT_LevSort TO TT_LevSort.

    RELEASE TT_LevSort.
    DELETE tmpTT_LevSort.
  END. /* LOOPEN */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesLokalGruppering1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesLokalGruppering1.0 Procedure 
PROCEDURE lesLokalGruppering1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmptt_LokalGruppering.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_LokalGruppering.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_LokalGruppering.LGId          
      tmpTT_LokalGruppering.LGBeskrivelse      
      tmpTT_LokalGruppering.EDato         
      tmpTT_LokalGruppering.ETid          
      tmpTT_LokalGruppering.BrukerID      
      tmpTT_LokalGruppering.RegistrertDato
      tmpTT_LokalGruppering.RegistrertTid 
      tmpTT_LokalGruppering.RegistrertAv
      .
    FIND TT_LokalGruppering WHERE
        TT_LokalGruppering.LGId = tmpTT_LokalGruppering.LGId NO-ERROR.
    IF NOT AVAILABLE TT_LokalGruppering THEN
        CREATE TT_LokalGruppering.
    BUFFER-COPY tmpTT_LokalGruppering TO TT_LokalGruppering.

    RELEASE TT_LokalGruppering.
    DELETE tmpTT_LokalGruppering.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesMaterial1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesMaterial1.0 Procedure 
PROCEDURE lesMaterial1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Material.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_Material.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_Material.MatKod        
      tmpTT_Material.MatBeskr      
      tmpTT_Material.EDato         
      tmpTT_Material.ETid          
      tmpTT_Material.BrukerID      
      tmpTT_Material.RegistrertDato
      tmpTT_Material.RegistrertTid 
      tmpTT_Material.RegistrertAv
      .
    FIND TT_Material WHERE 
        TT_Material.MatKod  = tmpTT_Material.MatKod
        NO-ERROR.
    IF NOT AVAILABLE TT_Material THEN
        CREATE TT_Material.
    BUFFER-COPY tmpTT_Material TO TT_Material.

    RELEASE TT_Material.
    DELETE tmpTT_Material.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesMesse1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesMesse1.0 Procedure 
PROCEDURE lesMesse1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmptt_Messe.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_Messe.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_Messe.MesseNr           
      tmpTT_Messe.MesseBeskrivelse
      tmpTT_Messe.EDato         
      tmpTT_Messe.ETid          
      tmpTT_Messe.BrukerID      
      tmpTT_Messe.RegistrertDato
      tmpTT_Messe.RegistrertTid 
      tmpTT_Messe.RegistrertAv
      .
    FIND TT_Messe WHERE
        TT_Messe.MesseNr = tmpTT_Messe.MesseNr NO-ERROR.
    IF NOT AVAILABLE TT_Messe THEN
        CREATE TT_Messe.
    BUFFER-COPY tmpTT_Messe TO TT_Messe.

    RELEASE TT_Messe.
    DELETE tmpTT_Messe.
  END. /* LOOPEN */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesMoms1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesMoms1.0 Procedure 
PROCEDURE lesMoms1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Moms.
    ASSIGN
      tmpTT_Moms.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_Moms.MomsKod       
      tmpTT_Moms.MomsProc      
      tmpTT_Moms.Beskrivelse   
      tmpTT_Moms.EDato         
      tmpTT_Moms.ETid          
      tmpTT_Moms.BrukerID      
      tmpTT_Moms.RegistrertDato
      tmpTT_Moms.RegistrertTid 
      tmpTT_Moms.RegistrertAv
      .
    FIND TT_Moms WHERE 
        TT_Moms.MomsKod  = tmpTT_Moms.MomsKod
        NO-ERROR.
    IF NOT AVAILABLE TT_Moms THEN
        CREATE TT_Moms.
    BUFFER-COPY tmpTT_Moms TO TT_Moms.

    RELEASE TT_Moms.
    DELETE tmpTT_Moms.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesOvandel1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesOvandel1.0 Procedure 
PROCEDURE lesOvandel1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Ovandel.
    ASSIGN
      tmpTT_Ovandel.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_Ovandel.Ov-Id         
      tmpTT_Ovandel.OvBeskr       
      tmpTT_Ovandel.EDato         
      tmpTT_Ovandel.ETid          
      tmpTT_Ovandel.BrukerID      
      tmpTT_Ovandel.RegistrertDato
      tmpTT_Ovandel.RegistrertTid 
      tmpTT_Ovandel.RegistrertAv.
      .
      FIND TT_Ovandel WHERE 
          TT_Ovandel.Ov-Id  = tmpTT_Ovandel.Ov-Id
          NO-ERROR.
      IF NOT AVAILABLE TT_Ovandel THEN
          CREATE TT_Ovandel.
      BUFFER-COPY tmpTT_Ovandel TO TT_Ovandel.

      RELEASE TT_Ovandel.
      DELETE tmpTT_Ovandel.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesPost1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesPost1.0 Procedure 
PROCEDURE lesPost1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Post.
    ASSIGN
      tmpTT_Post.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_Post.PostNr        
      tmpTT_Post.KommNr        
      tmpTT_Post.Beskrivelse   
      tmpTT_Post.Merknad       
      tmpTT_Post.FylkesNr      
      tmpTT_Post.EDato         
      tmpTT_Post.ETid          
      tmpTT_Post.BrukerID      
      tmpTT_Post.RegistrertDato
      tmpTT_Post.RegistrertTid 
      tmpTT_Post.RegistrertAv
      .
    FIND TT_Post WHERE 
        TT_Post.PostNr  = tmpTT_Post.PostNr
        NO-ERROR.
    IF NOT AVAILABLE TT_Post THEN
        CREATE TT_Post.
    BUFFER-COPY tmpTT_Post TO TT_Post.

    RELEASE TT_Post.
    DELETE tmpTT_Post.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesPrisgruppe1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesPrisgruppe1.0 Procedure 
PROCEDURE lesPrisgruppe1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Prisgruppe.
    ASSIGN
      tmpTT_Prisgruppe.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_Prisgruppe.PrisGrpNr     
      tmpTT_Prisgruppe.Beskrivelse   
      tmpTT_Prisgruppe.FraPris       
      tmpTT_Prisgruppe.TilPris       
      tmpTT_Prisgruppe.EDato         
      tmpTT_Prisgruppe.ETid          
      tmpTT_Prisgruppe.BrukerID      
      tmpTT_Prisgruppe.RegistrertDato
      tmpTT_Prisgruppe.RegistrertTid 
      tmpTT_Prisgruppe.RegistrertAv
      .
    FIND TT_Prisgruppe WHERE 
        TT_Prisgruppe.PrisGrpNr  = tmpTT_Prisgruppe.PrisGrpNr
        NO-ERROR.
    IF NOT AVAILABLE TT_Prisgruppe THEN
        CREATE TT_Prisgruppe.
    BUFFER-COPY tmpTT_Prisgruppe TO TT_Prisgruppe.

    RELEASE TT_Prisgruppe.
    DELETE tmpTT_Prisgruppe.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesPrisprofil1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesPrisprofil1.0 Procedure 
PROCEDURE lesPrisprofil1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Prisprofil.
    ASSIGN
      tmpTT_Prisprofil.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_Prisprofil.EDato         
      tmpTT_Prisprofil.ETid          
      tmpTT_Prisprofil.BrukerID      
      tmpTT_Prisprofil.RegistrertDato
      tmpTT_Prisprofil.RegistrertTid 
      tmpTT_Prisprofil.RegistrertAv  
      tmpTT_Prisprofil.ProfilNr      
      tmpTT_Prisprofil.KortNavn      
      tmpTT_Prisprofil.Beskrivelse   
      tmpTT_Prisprofil.Merknad       
      tmpTT_Prisprofil.Notat.
      .
      FIND TT_Prisprofil WHERE 
          TT_Prisprofil.ProfilNr  = tmpTT_Prisprofil.ProfilNr
          NO-ERROR.
      IF NOT AVAILABLE TT_Prisprofil THEN
          CREATE TT_Prisprofil.
      BUFFER-COPY tmpTT_Prisprofil TO TT_Prisprofil.

      RELEASE TT_Prisprofil.
      DELETE tmpTT_Prisprofil.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesProdusent1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesProdusent1.0 Procedure 
PROCEDURE lesProdusent1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Produsent.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_Produsent.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_Produsent.RegistrertDato
      tmpTT_Produsent.RegistrertTid 
      tmpTT_Produsent.EDato         
      tmpTT_Produsent.ETid          
      tmpTT_Produsent.BrukerID      
      tmpTT_Produsent.RegistrertAv  
      tmpTT_Produsent.Merknad       
      tmpTT_Produsent.Beskrivelse   
      tmpTT_Produsent.Notat         
      tmpTT_Produsent.Adresse1      
      tmpTT_Produsent.Adresse2      
      tmpTT_Produsent.PostNr        
      tmpTT_Produsent.PostBoks      
      tmpTT_Produsent.Telefon       
      tmpTT_Produsent.Kontakt       
      tmpTT_Produsent.Land          
      tmpTT_Produsent.ProdNr
      .
    FIND TT_Produsent WHERE 
        TT_Produsent.ProdNr  = tmpTT_Produsent.ProdNr
        NO-ERROR.
    IF NOT AVAILABLE TT_Produsent THEN
        CREATE TT_Produsent.
    BUFFER-COPY tmpTT_Produsent TO TT_Produsent.

    RELEASE TT_Produsent.
    DELETE tmpTT_Produsent.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesProgramListe1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesProgramListe1.0 Procedure 
PROCEDURE lesProgramListe1.0 :
/*------------------------------------------------------------------------------
  Purpose:     Innlesning av Programliste
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmptt_ProgramListe.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_ProgramListe.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_ProgramListe.ProgNavn           
      tmpTT_ProgramListe.Programbeskrivelse 
      tmpTT_ProgramListe.grad               
      tmpTT_ProgramListe.EDato         
      tmpTT_ProgramListe.ETid          
      tmpTT_ProgramListe.BrukerID      
      tmpTT_ProgramListe.RegistrertDato
      tmpTT_ProgramListe.RegistrertTid 
      tmpTT_ProgramListe.RegistrertAv
      .
    FIND TT_ProgramListe WHERE
        TT_ProgramListe.ProgNavn = tmpTT_ProgramListe.ProgNavn NO-ERROR.
    IF NOT AVAILABLE TT_ProgramListe THEN
        CREATE TT_ProgramListe.
    BUFFER-COPY tmpTT_ProgramListe TO TT_ProgramListe.

    RELEASE TT_ProgramListe.
    DELETE tmpTT_ProgramListe.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesProv1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesProv1.0 Procedure 
PROCEDURE lesProv1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Prov.
    ASSIGN
      tmpTT_Prov.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_Prov.ProvKod       
      tmpTT_Prov.ProvProc      
      tmpTT_Prov.ProvBeskr     
      tmpTT_Prov.EDato         
      tmpTT_Prov.ETid          
      tmpTT_Prov.BrukerID      
      tmpTT_Prov.RegistrertDato
      tmpTT_Prov.RegistrertTid 
      tmpTT_Prov.RegistrertAv.
      .
      FIND TT_Prov WHERE 
          TT_Prov.ProvKod  = tmpTT_Prov.ProvKod
          NO-ERROR.
      IF NOT AVAILABLE TT_Prov THEN
          CREATE TT_Prov.
      BUFFER-COPY tmpTT_Prov TO TT_Prov.

      RELEASE TT_Prov.
      DELETE tmpTT_Prov.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesRabatt1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesRabatt1.0 Procedure 
PROCEDURE lesRabatt1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Rabatt.
    ASSIGN
      tmpTT_Rabatt.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_Rabatt.RabKod        
      tmpTT_Rabatt.RabProc       
      tmpTT_Rabatt.RabBeskr      
      tmpTT_Rabatt.EDato         
      tmpTT_Rabatt.ETid          
      tmpTT_Rabatt.BrukerID      
      tmpTT_Rabatt.RegistrertDato
      tmpTT_Rabatt.RegistrertTid 
      tmpTT_Rabatt.RegistrertAv
      .
    FIND TT_Rabatt WHERE 
        TT_Rabatt.RabKod  = tmpTT_Rabatt.RabKod
        NO-ERROR.
    IF NOT AVAILABLE TT_Rabatt THEN
        CREATE TT_Rabatt.
    BUFFER-COPY tmpTT_Rabatt TO TT_Rabatt.

    RELEASE TT_Rabatt.
    DELETE tmpTT_Rabatt.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesSasong1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesSasong1.0 Procedure 
PROCEDURE lesSasong1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Sasong.
    ASSIGN
      tmpTT_Sasong.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_Sasong.Sasong        
      tmpTT_Sasong.SasBeskr      
      tmpTT_Sasong.EDato         
      tmpTT_Sasong.ETid          
      tmpTT_Sasong.BrukerID      
      tmpTT_Sasong.RegistrertDato
      tmpTT_Sasong.RegistrertTid 
      tmpTT_Sasong.RegistrertAv.
      .
      FIND TT_Sasong WHERE 
          TT_Sasong.Sasong  = tmpTT_Sasong.Sasong
          NO-ERROR.
      IF NOT AVAILABLE TT_Sasong THEN
          CREATE TT_Sasong.
      BUFFER-COPY tmpTT_Sasong TO TT_Sasong.

      RELEASE TT_Sasong.
      DELETE tmpTT_Sasong.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesSlitSula1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesSlitSula1.0 Procedure 
PROCEDURE lesSlitSula1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_SlitSula.
    ASSIGN
      tmpTT_SlitSula.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_SlitSula.Slit-Id       
      tmpTT_SlitSula.SlitBeskr     
      tmpTT_SlitSula.EDato         
      tmpTT_SlitSula.ETid          
      tmpTT_SlitSula.BrukerID      
      tmpTT_SlitSula.RegistrertDato
      tmpTT_SlitSula.RegistrertTid 
      tmpTT_SlitSula.RegistrertAv
      .
    FIND TT_SlitSula WHERE 
        TT_SlitSula.Slit-Id  = tmpTT_SlitSula.Slit-Id
        NO-ERROR.
    IF NOT AVAILABLE TT_SlitSula THEN
        CREATE TT_SlitSula.
    BUFFER-COPY tmpTT_SlitSula TO TT_SlitSula.

    RELEASE TT_SlitSula.
    DELETE tmpTT_SlitSula.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesStrKonv1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesStrKonv1.0 Procedure 
PROCEDURE lesStrKonv1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_StrKonv.
    ASSIGN
      tmpTT_StrKonv.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_StrKonv.StrKode       
      tmpTT_StrKonv.Storl         
      tmpTT_StrKonv.Merknad       
      tmpTT_StrKonv.EDato         
      tmpTT_StrKonv.ETid          
      tmpTT_StrKonv.BrukerID      
      tmpTT_StrKonv.RegistrertDato
      tmpTT_StrKonv.RegistrertTid 
      tmpTT_StrKonv.RegistrertAv
      tmpTT_StrKonv.SeqNr
      .
    FIND TT_StrKonv WHERE 
        TT_StrKonv.StrKode  = tmpTT_StrKonv.StrKode
        NO-ERROR.
    IF NOT AVAILABLE TT_StrKonv THEN
        CREATE TT_StrKonv.
    BUFFER-COPY tmpTT_StrKonv TO TT_StrKonv.

    RELEASE TT_StrKonv.
    DELETE tmpTT_StrKonv.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesStrTstr1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesStrTstr1.0 Procedure 
PROCEDURE lesStrTstr1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop         AS INT  NO-UNDO.
  DEF VAR piLoop2        AS INT  NO-UNDO.
  DEF VAR pcType         AS CHAR NO-UNDO.
  DEF VAR pi2RecType      AS INT  NO-UNDO. 
  DEF VAR pcTabellNavn   AS CHAR NO-UNDO. 
  DEF VAR pcRecVersion   AS CHAR NO-UNDO.
  DEF VAR pi2AntRecord    AS INT  NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_StrTStr.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_StrTStr.RecType = pi2RecType
      .
    IMPORT STREAM InnFil 
      tmpTT_StrTStr.StrTypeID     
      tmpTT_StrTStr.SoStorl       
      tmpTT_StrTStr.SeqNr         
      tmpTT_StrTStr.EDato         
      tmpTT_StrTStr.ETid          
      tmpTT_StrTStr.BrukerID      
      tmpTT_StrTStr.RegistrertDato
      tmpTT_StrTStr.RegistrertTid 
      tmpTT_StrTStr.RegistrertAv  
    .
    FIND TT_StrTStr WHERE
        TT_StrTStr.STrTypeID = tmpTT_StrTStr.StrTypeId AND
        TT_StrTStr.SeqNr     = tmpTT_StrTStr.SeqNr NO-ERROR.
    IF NOT AVAILABLE TT_StrTStr THEN
        CREATE TT_StrTStr.
    BUFFER-COPY tmpTT_StrTStr TO TT_StrTStr.

    RELEASE TT_StrTStr.
    DELETE tmpTT_StrTStr.
  END. /* LOOPEN */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesStrType1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesStrType1.0 Procedure 
PROCEDURE lesStrType1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop         AS INT  NO-UNDO.
  DEF VAR piLoop2        AS INT  NO-UNDO.
  DEF VAR pcType         AS CHAR NO-UNDO.
  DEF VAR pi2RecType      AS INT  NO-UNDO. 
  DEF VAR pcTabellNavn   AS CHAR NO-UNDO. 
  DEF VAR pcRecVersion   AS CHAR NO-UNDO.
  DEF VAR pi2AntRecord    AS INT  NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_StrType.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_StrType.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_StrType.StrTypeID     
      tmpTT_StrType.Beskrivelse   
      tmpTT_StrType.Intervall     
      tmpTT_StrType.Fordeling     
      tmpTT_StrType.KortNavn      
      tmpTT_StrType.EDato         
      tmpTT_StrType.ETid          
      tmpTT_StrType.BrukerID      
      tmpTT_StrType.RegistrertDato
      tmpTT_StrType.RegistrertTid 
      tmpTT_StrType.RegistrertAv  
      tmpTT_StrType.AlfaFordeling
      tmpTT_StrType.Hg
      tmpTT_StrType.AvdelingNr
      .

    FIND TT_StrType WHERE
        TT_StrType.StrTypeId = tmpTT_StrType.StrTypeId NO-ERROR.
    IF NOT AVAILABLE TT_StrType THEN
        CREATE TT_StrType.
    BUFFER-COPY tmpTT_StrType TO TT_StrType.

    RELEASE TT_StrType.
    DELETE tmpTT_StrType.
  END. /* LOOPEN */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesUtbetType1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesUtbetType1.0 Procedure 
PROCEDURE lesUtbetType1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmptt_UtbetType.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_UtbetType.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_UtbetType.UtBetTId       
      tmpTT_UtbetType.UtBBeskrivelse 
      tmpTT_UtbetType.KontoNr        
      tmpTT_UtbetType.EDato         
      tmpTT_UtbetType.ETid          
      tmpTT_UtbetType.BrukerID      
      tmpTT_UtbetType.RegistrertDato
      tmpTT_UtbetType.RegistrertTid 
      tmpTT_UtbetType.RegistrertAv
      .
    FIND TT_UtbetType WHERE
        TT_UtbetType.UtbetTId = tmpTT_UtbetType.UtbetTId NO-ERROR.
    IF NOT AVAILABLE TT_UtbetType THEN
        CREATE TT_UtbetType.
    BUFFER-COPY tmpTT_UtbetType TO TT_UtbetType.

    RELEASE TT_UtbetType.
    DELETE tmpTT_UtbetType.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesValuta1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesValuta1.0 Procedure 
PROCEDURE lesValuta1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Valuta.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_Valuta.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_Valuta.ValKod        
      tmpTT_Valuta.ValKurs       
      tmpTT_Valuta.ValLand       
      tmpTT_Valuta.ValDatum      
      tmpTT_Valuta.EDato         
      tmpTT_Valuta.ETid          
      tmpTT_Valuta.BrukerID      
      tmpTT_Valuta.RegistrertDato
      tmpTT_Valuta.RegistrertTid 
      tmpTT_Valuta.RegistrertAv  
      tmpTT_Valuta.ValNr         
      tmpTT_Valuta.ValNavn       
      tmpTT_Valuta.indeks        
      tmpTT_Valuta.retur         
      tmpTT_Valuta.KasseValKurs.
      .
      FIND TT_Valuta WHERE 
          TT_Valuta.ValKod  = tmpTT_Valuta.ValKod
          NO-ERROR.
      IF NOT AVAILABLE TT_Valuta THEN
          CREATE TT_Valuta.
      BUFFER-COPY tmpTT_Valuta TO TT_Valuta.

      RELEASE TT_Valuta.
      DELETE tmpTT_Valuta.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesVareBehType1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesVareBehType1.0 Procedure 
PROCEDURE lesVareBehType1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmptt_VareBehType.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_VareBehType.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_VareBehType.VareBehType           
      tmpTT_VareBehType.BeskrivelseVareBehType
      tmpTT_VareBehType.EDato         
      tmpTT_VareBehType.ETid          
      tmpTT_VareBehType.BrukerID      
      tmpTT_VareBehType.RegistrertDato
      tmpTT_VareBehType.RegistrertTid 
      tmpTT_VareBehType.RegistrertAv
      .
    FIND TT_VareBehType WHERE
        TT_VareBehType.VareBehType = tmpTT_VareBehType.VareBehType NO-ERROR.
    IF NOT AVAILABLE TT_VareBehType THEN
        CREATE TT_VareBehType.
    BUFFER-COPY tmpTT_VareBehType TO TT_VareBehType.

    RELEASE TT_VareBehType.
    DELETE tmpTT_VareBehType.
  END. /* LOOPEN */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesVareBokType1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesVareBokType1.0 Procedure 
PROCEDURE lesVareBokType1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmptt_VareBokType.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_VareBokType.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_VareBokType.VareBokType           
      tmpTT_VareBokType.BeskrivelseVareBokType
      tmpTT_VareBokType.EDato         
      tmpTT_VareBokType.ETid          
      tmpTT_VareBokType.BrukerID      
      tmpTT_VareBokType.RegistrertDato
      tmpTT_VareBokType.RegistrertTid 
      tmpTT_VareBokType.RegistrertAv
      .
    FIND TT_VareBokType WHERE
        TT_VareBokType.VareBokType = tmpTT_VareBokType.VareBokType NO-ERROR.
    IF NOT AVAILABLE TT_VareBokType THEN
        CREATE TT_VareBokType.
    BUFFER-COPY tmpTT_VareBokType TO TT_VareBokType.

    RELEASE TT_VareBokType.
    DELETE tmpTT_VareBokType.
  END. /* LOOPEN */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesVaremerke1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesVaremerke1.0 Procedure 
PROCEDURE lesVaremerke1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_Varemerke.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_Varemerke.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_Varemerke.VMId          
      tmpTT_Varemerke.Beskrivelse   
      tmpTT_Varemerke.Merknad       
      tmpTT_Varemerke.RegistrertDato
      tmpTT_Varemerke.RegistrertTid 
      tmpTT_Varemerke.EDato         
      tmpTT_Varemerke.ETid          
      tmpTT_Varemerke.BrukerID      
      tmpTT_Varemerke.RegistrertAv  
      tmpTT_Varemerke.KortNavn
      .
    FIND TT_Varemerke WHERE 
        TT_Varemerke.VmId  = tmpTT_Varemerke.VmId
        NO-ERROR.
    IF NOT AVAILABLE TT_Varemerke THEN
        CREATE TT_Varemerke.
    BUFFER-COPY tmpTT_Varemerke TO TT_Varemerke.

    RELEASE TT_Varemerke.
    DELETE tmpTT_Varemerke.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesVarGr1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesVarGr1.0 Procedure 
PROCEDURE lesVarGr1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_VarGr.
    ASSIGN
      tmpTT_VarGr.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_VarGr.Vg            
      tmpTT_VarGr.VgBeskr       
      tmpTT_VarGr.StoArt        
      tmpTT_VarGr.MomsKod       
      tmpTT_VarGr.Hg            
      tmpTT_VarGr.Kost_Proc     
      tmpTT_VarGr.Kolonne       
      tmpTT_VarGr.EDato         
      tmpTT_VarGr.ETid          
      tmpTT_VarGr.BrukerID      
      tmpTT_VarGr.RegistrertDato
      tmpTT_VarGr.RegistrertTid 
      tmpTT_VarGr.RegistrertAv
      .
    FIND TT_VarGr WHERE 
        TT_VarGr.Vg  = tmpTT_VarGr.Vg
        NO-ERROR.
    IF NOT AVAILABLE TT_VarGr THEN
        CREATE TT_VarGr.
    BUFFER-COPY tmpTT_VarGr TO TT_VarGr.

    RELEASE TT_VarGr.
    DELETE tmpTT_VarGr.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesVgAkt1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesVgAkt1.0 Procedure 
PROCEDURE lesVgAkt1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmptt_VgAkt.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_VgAkt.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_VgAkt.Vg          
      tmpTT_VgAkt.AktNr      
      tmpTT_VgAkt.EDato         
      tmpTT_VgAkt.ETid          
      tmpTT_VgAkt.BrukerID      
      tmpTT_VgAkt.RegistrertDato
      tmpTT_VgAkt.RegistrertTid 
      tmpTT_VgAkt.RegistrertAv
      .
    FIND TT_VgAkt WHERE
        TT_VgAkt.Vg    = tmpTT_VgAkt.Vg AND
        TT_VgAkt.AktNr = tmpTT_VgAkt.AktNr NO-ERROR.
    IF NOT AVAILABLE TT_VgAkt THEN
        CREATE TT_VgAkt.
    BUFFER-COPY tmpTT_VgAkt TO TT_VgAkt.

    RELEASE TT_VgAkt.
    DELETE tmpTT_VgAkt.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesVgKat1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesVgKat1.0 Procedure 
PROCEDURE lesVgKat1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_VgKat.
    ASSIGN
      tmpTT_VgKat.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_VgKat.Vg            
      tmpTT_VgKat.VgKat         
      tmpTT_VgKat.KatNr         
      tmpTT_VgKat.EDato         
      tmpTT_VgKat.ETid          
      tmpTT_VgKat.BrukerID      
      tmpTT_VgKat.RegistrertDato
      tmpTT_VgKat.RegistrertTid 
      tmpTT_VgKat.RegistrertAv
      .
    FIND TT_VgKat WHERE 
        TT_VgKat.Vg     = tmpTT_VgKat.Vg AND
        TT_VgKat.VgKat  = tmpTT_VgKat.VgKat AND
        TT_VgKat.KatNr  = tmpTT_VgKat.KatNr 
        NO-ERROR.
    IF NOT AVAILABLE TT_VgKat THEN
        CREATE TT_VgKat.
    BUFFER-COPY tmpTT_VgKat TO TT_VgKat.

    RELEASE TT_VgKat.
    DELETE tmpTT_VgKat.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesVPIAltLevBas1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesVPIAltLevBas1.0 Procedure 
PROCEDURE lesVPIAltLevBas1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_VPIAltLevBas.
    ASSIGN
      tmpTT_VPIAltLevBas.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_VPIAltLevBas.EkstVPILevNr    
      tmpTT_VPIAltLevBas.VareNr          
      tmpTT_VPIAltLevBas.LevNr         
      tmpTT_VPIAltLevBas.EDato         
      tmpTT_VPIAltLevBas.ETid          
      tmpTT_VPIAltLevBas.BrukerID      
      tmpTT_VPIAltLevBas.RegistrertDato
      tmpTT_VPIAltLevBas.RegistrertTid 
      tmpTT_VPIAltLevBas.RegistrertAv  
      .
    FIND TT_VPIAltLevBas WHERE 
        TT_VPIAltLevBas.EkstVPILevNr = tmpTT_VPIAltLevBas.EkstVPILevNr AND
        TT_VPIAltLevBas.VareNr       = tmpTT_VPIAltLevBas.VareNr AND
        TT_VPIAltLevBas.LevNr        = tmpTT_VPIAltLevBas.LevNr
        NO-ERROR.
    IF NOT AVAILABLE TT_VPIAltLevBas THEN
        CREATE TT_VPIAltLevBas.
    BUFFER-COPY tmpTT_VPIAltLevBas TO TT_VPIAltLevBas.

    RELEASE TT_VPIAltLevBas.
    DELETE tmpTT_VPIAltLevBas.
  END. /* LOOPEN */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesVPIArtBestPkt1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesVPIArtBestPkt1.0 Procedure 
PROCEDURE lesVPIArtBestPkt1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.
  
/* TN 4/9-09

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_VPIArtBestPkt.
    ASSIGN
      tmpTT_VPIArtBestPkt.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_VPIArtBestPkt.EkstVPILevNr    
      tmpTT_VPIArtBestPkt.VareNr          
      tmpTT_VPIArtBestPkt.ArtikkelNr     
      tmpTT_VPIArtBestPkt.ButikkNr         
      tmpTT_VPIArtBestPkt.StrKode          
      tmpTT_VPIArtBestPkt.MaksAnt          
      tmpTT_VPIArtBestPkt.MinAnt           
      tmpTT_VPIArtBestPkt.BestAnt          
      tmpTT_VPIArtBestPkt.TillatBruttPk    
      tmpTT_VPIArtBestPkt.EDato         
      tmpTT_VPIArtBestPkt.ETid          
      tmpTT_VPIArtBestPkt.BrukerID      
      tmpTT_VPIArtBestPkt.RegistrertDato
      tmpTT_VPIArtBestPkt.RegistrertTid 
      tmpTT_VPIArtBestPkt.RegistrertAv  
      .
    FIND TT_VPIArtBestPkt WHERE 
        TT_VPIArtBestPkt.EkstVPILevNr = tmpTT_VPIArtBestPkt.EkstVPILevNr AND
        TT_VPIArtBestPkt.VareNr       = tmpTT_VPIArtBestPkt.VareNr AND
        TT_VPIArtBestPkt.StrKode      = tmpTT_VPIArtBestPkt.StrKode
        NO-ERROR.
    IF NOT AVAILABLE TT_VPIArtBestPkt THEN
        CREATE TT_VPIArtBestPkt.
    BUFFER-COPY tmpTT_VPIArtBestPkt TO TT_VPIArtBestPkt.

    RELEASE TT_VPIArtBestPkt.
    DELETE tmpTT_VPIArtBestPkt.
  END. /* LOOPEN */
  
*/
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesVPIBildeData1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesVPIBildeData1.0 Procedure 
PROCEDURE lesVPIBildeData1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_VPIBildedata.
    ASSIGN
      tmpTT_VPIBildedata.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil
        tmpTT_VPIBildeData.EkstVPILevNr   
        tmpTT_VPIBildeData.VareNr         
        tmpTT_VPIBildeData.BildNr         
        tmpTT_VPIBildeData.Teller         
        tmpTT_VPIBildeData.RawData        
        tmpTT_VPIBildeData.EDato          
        tmpTT_VPIBildeData.ETid           
        tmpTT_VPIBildeData.BrukerID       
        tmpTT_VPIBildeData.RegistrertDato 
        tmpTT_VPIBildeData.RegistrertTid  
        tmpTT_VPIBildeData.RegistrertAv   
      .
    FIND TT_VPIBildeData WHERE 
        TT_VPIBildeData.EkstVPILevNr = tmpTT_VPIBildeData.EkstVPILevNr AND
        TT_VPIBildeData.VareNr       = tmpTT_VPIBildeData.VareNr AND
        TT_VPIBildeData.BildNr       = tmpTT_VPIBildeData.BildNr AND
        TT_VPIBildeData.Teller       = tmpTT_VPIBildeData.Teller
        NO-ERROR.
    IF NOT AVAILABLE TT_VPIBildeData THEN
        CREATE TT_VPIBildeData.
    BUFFER-COPY tmpTT_VPIBildeData TO TT_VPIBildeData.

    RELEASE TT_VPIBildeData.
    DELETE tmpTT_VPIBildeData.
  END. /* LOOPEN */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesVPIBildeRegister1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesVPIBildeRegister1.0 Procedure 
PROCEDURE lesVPIBildeRegister1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_VPIBilderegister.
    ASSIGN
      tmpTT_VPIBilderegister.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil
        tmpTT_VPIBilderegister.EkstVPILevNr 
        tmpTT_VPIBilderegister.VareNr
        tmpTT_VPIBilderegister.BildNr         
        tmpTT_VPIBilderegister.Merknad        
        tmpTT_VPIBilderegister.Tekst          
        tmpTT_VPIBilderegister.FilNavn        
        tmpTT_VPIBilderegister.RegistrertDato 
        tmpTT_VPIBilderegister.Dato           
        tmpTT_VPIBilderegister.Notat          
        tmpTT_VPIBilderegister.LevArtNr       
        tmpTT_VPIBilderegister.LevNr          
        tmpTT_VPIBilderegister.RegistrertTid  
        tmpTT_VPIBilderegister.Tid            
        tmpTT_VPIBilderegister.Sted           
        tmpTT_VPIBilderegister.EDato          
        tmpTT_VPIBilderegister.ETid           
        tmpTT_VPIBilderegister.BrukerID       
        tmpTT_VPIBilderegister.EksterntID     
        tmpTT_VPIBilderegister.RegistrertAv   
        tmpTT_VPIBilderegister.DokumentNr     
        tmpTT_VPIBilderegister.Bytes          
      .
    FIND TT_VPIBilderegister WHERE 
        TT_VPIBilderegister.EkstVPILevNr = tmpTT_VPIBilderegister.EkstVPILevNr AND
        TT_VPIBilderegister.VareNr       = tmpTT_VPIBilderegister.VareNr AND
        TT_VPIBilderegister.BildNr       = tmpTT_VPIBilderegister.BildNr
        NO-ERROR.
    IF NOT AVAILABLE TT_VPIBilderegister THEN
        CREATE TT_VPIBilderegister.
    BUFFER-COPY tmpTT_VPIBilderegister TO TT_VPIBilderegister.

    RELEASE TT_VPIBilderegister.
    DELETE tmpTT_VPIBilderegister.
  END. /* LOOPEN */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesVPIErstattningsvare1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesVPIErstattningsvare1.0 Procedure 
PROCEDURE lesVPIErstattningsvare1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_VPIErstattningsvare.
    ASSIGN
      tmpTT_VPIErstattningsvare.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
        tmpTT_VPIErstattningsvare.ErstattId    
        tmpTT_VPIErstattningsvare.ArtikkelNr   
        tmpTT_VPIErstattningsvare.EkstVPILevNr 
        tmpTT_VPIErstattningsvare.VareNr       
      .
    FIND TT_VPIErstattningsvare WHERE 
        TT_VPIErstattningsvare.EkstVPILevNr = tmpTT_VPIErstattningsvare.EkstVPILevNr AND
        TT_VPIErstattningsvare.VareNr       = tmpTT_VPIErstattningsvare.VareNr AND
        TT_VPIErstattningsvare.ArtikkelNr   = tmpTT_VPIErstattningsvare.ArtikkelNr
        NO-ERROR.
    IF NOT AVAILABLE TT_VPIErstattningsvare THEN
        CREATE TT_VPIErstattningsvare.
    BUFFER-COPY tmpTT_VPIErstattningsvare TO TT_VPIErstattningsvare.

    RELEASE TT_VPIErstattningsvare.
    DELETE tmpTT_VPIErstattningsvare.
  END. /* LOOPEN */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesVPIPakkeLinje1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesVPIPakkeLinje1.0 Procedure 
PROCEDURE lesVPIPakkeLinje1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_VPIPakkelinje.
    ASSIGN
      tmpTT_VPIPakkelinje.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
        tmpTT_VPIPakkelinje.ArtikkelNr     
        tmpTT_VPIPakkelinje.PkArtikkelNr   
        tmpTT_VPIPakkelinje.StrKode           
        tmpTT_VPIPakkelinje.Antall         
        tmpTT_VPIPakkelinje.EDato          
        tmpTT_VPIPakkelinje.ETid           
        tmpTT_VPIPakkelinje.BrukerID       
        tmpTT_VPIPakkelinje.RegistrertDato 
        tmpTT_VPIPakkelinje.RegistrertTid  
        tmpTT_VPIPakkelinje.RegistrertAv   
        tmpTT_VPIPakkelinje.Pakkenr        
        tmpTT_VPIPakkelinje.EkstVPILevNr   
        tmpTT_VPIPakkelinje.VareNr
        tmpTT_VPIPakkeLinje.VareKost
      .
    FIND TT_VPIPakkelinje WHERE 
        TT_VPIPakkelinje.EkstVPILevNr = tmpTT_VPIPakkelinje.EkstVPILevNr AND
        TT_VPIPakkelinje.VareNr       = tmpTT_VPIPakkelinje.VareNr AND
        TT_VPIPakkelinje.ArtikkelNr   = tmpTT_VPIPakkelinje.ArtikkelNr AND
        TT_VPIPakkelinje.PkArtikkelNr = tmpTT_VPIPakkelinje.PkArtikkelNr AND
        TT_VPIPakkeLinje.StrKode      = tmpTT_VPIPakkeLinje.StrKode
        NO-ERROR.
    IF NOT AVAILABLE TT_VPIPakkelinje THEN
        CREATE TT_VPIPakkelinje.
    BUFFER-COPY tmpTT_VPIPakkelinje TO TT_VPIPakkelinje.

    RELEASE TT_VPIPakkelinje.
    DELETE tmpTT_VPIPakkelinje.
  END. /* LOOPEN */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-lesVPIStrekkode1.0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE lesVPIStrekkode1.0 Procedure 
PROCEDURE lesVPIStrekkode1.0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmpTT_VPIStrekkode.
    ASSIGN
      tmpTT_VPIStrekkode.RecType = piRecType
      iAntLinjer = iAntLinjer + 1
      .
    IMPORT STREAM InnFil 
      tmpTT_VPIStrekkode.EkstVPILevNr    
      tmpTT_VPIStrekkode.VareNr          
      tmpTT_VPIStrekkode.Kode          
      tmpTT_VPIStrekkode.StrKode       
      tmpTT_VPIStrekkode.KodeType      
      tmpTT_VPIStrekkode.VareId        
      tmpTT_VPIStrekkode.HovedNr       
      tmpTT_VPIStrekkode.EDato         
      tmpTT_VPIStrekkode.ETid          
      tmpTT_VPIStrekkode.BrukerID      
      tmpTT_VPIStrekkode.RegistrertDato
      tmpTT_VPIStrekkode.RegistrertTid 
      tmpTT_VPIStrekkode.RegistrertAv  
      tmpTT_VPIStrekkode.EkstStorl     
      tmpTT_VPIStrekkode.Storl         
      tmpTT_VPIStrekkode.Bestillingsnummer
      .
    IMPORT STREAM InnFil
      .
    /* Trimmer av eventuelle space. */
    ASSIGN
        tmpTT_VPIStrekkode.Kode = TRIM(tmpTT_VPIStrekkode.Kode)
        .
    /* Tar den bort og legger den opp pånytt. Den kan ha endret størrelse. */
    /* eller den kan ligge på en annen artikkel.                           */
    FOR EACH TT_VPIStrekkode WHERE 
        TT_VPIStrekkode.EkstVPILevNr = tmpTT_VPIStrekkode.EkstVPILevNr AND
        TT_VPIStrekkode.Kode         = tmpTT_VPIStrekkode.Kode:
        DELETE tt_VPISTrekkode.
    END.
    /* Legger opp en ny frisk strekkode */
    IF NOT AVAILABLE TT_VPIStrekkode THEN
        CREATE TT_VPIStrekkode.
    BUFFER-COPY tmpTT_VPIStrekkode TO TT_VPIStrekkode.

    RELEASE TT_VPIStrekkode.
    DELETE tmpTT_VPIStrekkode.
  END. /* LOOPEN */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdAktivitet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdAktivitet Procedure 
PROCEDURE oppdAktivitet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Aktivitet) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF Aktivitet OVERRIDE 
  DO:  
  END.
  ON WRITE OF Aktivitet OVERRIDE 
  DO:  
  END.
  ON DELETE OF Aktivitet OVERRIDE 
  DO:  
  END.
  */

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Aktivitet WHERE
    TT_Aktivitet.RecType = 3:
    FIND Aktivitet EXCLUSIVE-LOCK WHERE
      Aktivitet.AktNr = TT_Aktivitet.AktNr NO-ERROR.
    IF AVAILABLE Aktivitet THEN
    DO:
      /*IF NOT CAN-FIND(FIRST VgAkt OF Aktivitet) THEN*/
      DO:
        FOR EACH VgAkt OF Aktivitet:
            DELETE VgAkt.
        END.
        DELETE Aktivitet NO-ERROR.
        DELETE TT_Aktivitet.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Aktivitet WHERE
    TT_Aktivitet.RecType = 1:
    FIND Aktivitet EXCLUSIVE-LOCK WHERE
      Aktivitet.AktNr = TT_Aktivitet.AktNr NO-ERROR.
    IF NOT AVAILABLE Aktivitet THEN
      CREATE Aktivitet.
    BUFFER-COPY TT_Aktivitet TO Aktivitet.
    RELEASE Aktivitet.
    DELETE TT_Aktivitet.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdAltLevBas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdAltLevBas Procedure 
PROCEDURE oppdAltLevBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER piRecType   AS INT NO-UNDO.
  DEF INPUT PARAMETER piAntRecord AS INT NO-UNDO.

  DEF VAR piLoop AS INT NO-UNDO.

  LOOPEN:
  DO piLoop = 1 TO piAntRecord:
    CREATE tmptt_AltLevBas.
    ASSIGN
      iAntLinjer = iAntLinjer + 1
      tmpTT_AltLevBas.RecType = piRecType
      .
    IMPORT STREAM InnFil 
      tmpTT_AltLevBas.ArtikkelNr          
      tmpTT_AltLevBas.LevNr      
      tmpTT_AltLevBas.EDato         
      tmpTT_AltLevBas.ETid          
      tmpTT_AltLevBas.BrukerID      
      tmpTT_AltLevBas.RegistrertDato
      tmpTT_AltLevBas.RegistrertTid 
      tmpTT_AltLevBas.RegistrertAv
      .
    FIND TT_AltLevBas WHERE
        TT_AltLevBas.ArtikkelNr = tmpTT_AltLevBas.ArtikkelNr AND
        TT_AltLevBas.LevNr      = tmpTT_AltLevBas.LevNr NO-ERROR.
    IF NOT AVAILABLE TT_AltLevBas THEN
        CREATE TT_AltLevBas.
    BUFFER-COPY tmpTT_AltLevBas TO TT_AltLevBas.

    RELEASE TT_AltLevBas.
    DELETE tmpTT_AltLevBas.
  END. /* LOOPEN */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdAnv-Kod) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdAnv-Kod Procedure 
PROCEDURE oppdAnv-Kod :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Anv-Kod) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  ON CREATE OF Anv-Kod OVERRIDE 
  DO:  
  END.
  ON WRITE OF Anv-Kod OVERRIDE 
  DO:  
  END.
  ON DELETE OF Anv-Kod OVERRIDE 
  DO:  
  END.
  
  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Anv-Kod WHERE
    TT_Anv-Kod.RecType = 3:
    FIND Anv-Kod EXCLUSIVE-LOCK WHERE
      Anv-Kod.Anv-Id = TT_Anv-Kod.Anv-Id NO-ERROR.
    IF AVAILABLE Anv-Kod THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF Anv-Kod) THEN
      DO:
        DELETE Anv-Kod NO-ERROR.
        DELETE TT_Anv-Kod.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Anv-Kod WHERE
    TT_Anv-Kod.RecType = 1:
    FIND Anv-Kod EXCLUSIVE-LOCK WHERE
      Anv-Kod.Anv-Id = TT_Anv-Kod.Anv-Id NO-ERROR.
    IF NOT AVAILABLE Anv-Kod THEN
      CREATE Anv-Kod.
    BUFFER-COPY TT_Anv-Kod TO Anv-Kod.
    RELEASE Anv-Kod.
    DELETE TT_Anv-Kod.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdArtBestPkt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdArtBestPkt Procedure 
PROCEDURE oppdArtBestPkt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* TN 4/9-09

  IF NOT CAN-FIND(FIRST TT_ArtBestPkt) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF ArtBestPkt OVERRIDE 
  DO:  
  END.
  ON WRITE OF ArtBestPkt OVERRIDE 
  DO:  
  END.
  ON DELETE OF ArtBestPkt OVERRIDE 
  DO:  
  END.
  */

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_ArtBestPkt WHERE
    TT_ArtBestPkt.RecType = 3:
    FIND ArtBestPkt EXCLUSIVE-LOCK WHERE
      ArtBestPkt.ArtikkelNr = TT_ArtBestPkt.ArtikkelNr AND
      ArtBestPkt.ButikkNr   = TT_ArtBestPkt.ButikkNr AND
      ArtBestPkt.StrKode    = TT_ArtBestPkt.StrKode NO-ERROR.
    IF AVAILABLE ArtBestPkt THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF ArtBestPkt) THEN
      DO:
        DELETE ArtBestPkt NO-ERROR.
        DELETE TT_ArtBestPkt.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_ArtBestPkt WHERE
    TT_ArtBestPkt.RecType = 1:
    FIND ArtBestPkt EXCLUSIVE-LOCK WHERE
      ArtBestPkt.ArtikkelNr = TT_ArtBestPkt.ArtikkelNr AND
      ArtBestPkt.ButikkNr   = TT_ArtBestPkt.ButikkNr AND
      ArtBestPkt.StrKode    = TT_ArtBestPkt.StrKode NO-ERROR.
    IF NOT AVAILABLE ArtBestPkt THEN
      CREATE ArtBestPkt.
    BUFFER-COPY TT_ArtBestPkt TO ArtBestPkt.
    RELEASE ArtBestPkt.
    DELETE TT_ArtBestPkt.
  END. /* NY-ENDRE */
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdAvdeling) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdAvdeling Procedure 
PROCEDURE oppdAvdeling :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Avdeling) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF Avdeling OVERRIDE 
  DO:  
  END.
  ON WRITE OF Avdeling OVERRIDE 
  DO:  
  END.
  ON DELETE OF Avdeling OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Avdeling WHERE
    TT_Avdeling.RecType = 3:
    FIND Avdeling EXCLUSIVE-LOCK WHERE
      Avdeling.AvdelingNr = TT_Avdeling.AvdelingNr NO-ERROR.
    IF AVAILABLE Avdeling THEN
    DO:
      IF NOT CAN-FIND(FIRST HuvGr OF Avdeling) THEN
      DO:
        DELETE Avdeling NO-ERROR.
        DELETE TT_Avdeling.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Avdeling WHERE
    TT_Avdeling.RecType = 1:
    FIND Avdeling EXCLUSIVE-LOCK WHERE
      Avdeling.AvdelingNr = TT_Avdeling.AvdelingNr NO-ERROR.
    IF NOT AVAILABLE Avdeling THEN
      CREATE Avdeling.
    BUFFER-COPY TT_Avdeling TO Avdeling.
    RELEASE Avdeling.
    DELETE TT_Avdeling.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdBehandlingskode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdBehandlingskode Procedure 
PROCEDURE oppdBehandlingskode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Behandlingskode) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF Behandlingskode OVERRIDE 
  DO:  
  END.
  ON WRITE OF Behandlingskode OVERRIDE 
  DO:  
  END.
  ON DELETE OF Behandlingskode OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Behandlingskode WHERE
    TT_Behandlingskode.RecType = 3:
    FIND Behandlingskode EXCLUSIVE-LOCK WHERE
      Behandlingskode.BehKode = TT_Behandlingskode.BehKode NO-ERROR.
    IF AVAILABLE Behandlingskode THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF Behandlingskode) THEN
      DO:
        DELETE Behandlingskode NO-ERROR.
        DELETE TT_Behandlingskode.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Behandlingskode WHERE
    TT_Behandlingskode.RecType = 1:
    FIND Behandlingskode EXCLUSIVE-LOCK WHERE
      Behandlingskode.BehKode = TT_Behandlingskode.BehKode NO-ERROR.
    IF NOT AVAILABLE Behandlingskode THEN
      CREATE Behandlingskode.
    BUFFER-COPY TT_Behandlingskode TO Behandlingskode.
    RELEASE Behandlingskode.
    DELETE TT_Behandlingskode.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdBeliggenhet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdBeliggenhet Procedure 
PROCEDURE oppdBeliggenhet :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Beliggenhet) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF Beliggenhet OVERRIDE 
  DO:  
  END.
  ON WRITE OF Beliggenhet OVERRIDE 
  DO:  
  END.
  ON DELETE OF Beliggenhet OVERRIDE 
  DO:  
  END.
  */

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Beliggenhet WHERE
    TT_Beliggenhet.RecType = 3:
    FIND Beliggenhet EXCLUSIVE-LOCK WHERE
      Beliggenhet.BeliggenhetId = TT_Beliggenhet.BeliggenhetId NO-ERROR.
    IF AVAILABLE Beliggenhet THEN
    DO:
      IF NOT CAN-FIND(FIRST KjedensButikker OF Beliggenhet) THEN
      DO:
        DELETE Beliggenhet NO-ERROR.
        DELETE TT_Beliggenhet.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Beliggenhet WHERE
    TT_Beliggenhet.RecType = 1:
    FIND Beliggenhet EXCLUSIVE-LOCK WHERE
      Beliggenhet.BeliggenhetId = TT_Beliggenhet.BeliggenhetId NO-ERROR.
    IF NOT AVAILABLE Beliggenhet THEN
      CREATE Beliggenhet.
    BUFFER-COPY TT_Beliggenhet TO Beliggenhet.
    RELEASE Beliggenhet.
    DELETE TT_Beliggenhet.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdDriftsForm) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdDriftsForm Procedure 
PROCEDURE oppdDriftsForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Driftsform) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF Driftsform OVERRIDE 
  DO:  
  END.
  ON WRITE OF Driftsform OVERRIDE 
  DO:  
  END.
  ON DELETE OF Driftsform OVERRIDE 
  DO:  
  END.
  */

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Driftsform WHERE
    TT_Driftsform.RecType = 3:
    FIND Driftsform EXCLUSIVE-LOCK WHERE
      Driftsform.DriftsFormId = TT_Driftsform.DriftsFormId NO-ERROR.
    IF AVAILABLE Driftsform THEN
    DO:
      IF NOT CAN-FIND(FIRST KjedensButikker OF Driftsform) THEN
      DO:
        DELETE Driftsform NO-ERROR.
        DELETE TT_Driftsform.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Driftsform WHERE
    TT_Driftsform.RecType = 1:
    FIND Driftsform EXCLUSIVE-LOCK WHERE
      Driftsform.DriftsFormId = TT_Driftsform.DriftsFormId NO-ERROR.
    IF NOT AVAILABLE Driftsform THEN
      CREATE Driftsform.
    BUFFER-COPY TT_Driftsform TO Driftsform.
    RELEASE Driftsform.
    DELETE TT_Driftsform.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdDriftsType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdDriftsType Procedure 
PROCEDURE oppdDriftsType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_DriftsType) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF DriftsType OVERRIDE 
  DO:  
  END.
  ON WRITE OF DriftsType OVERRIDE 
  DO:  
  END.
  ON DELETE OF DriftsType OVERRIDE 
  DO:  
  END.
  */

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_DriftsType WHERE
    TT_DriftsType.RecType = 3:
    FIND DriftsType EXCLUSIVE-LOCK WHERE
      DriftsType.DriftsFormId = TT_DriftsType.DriftsFormId AND
      DriftsType.DriftsTypeId = TT_DriftsType.DriftsTypeId NO-ERROR.
    IF AVAILABLE DriftsType THEN
    DO:
      IF NOT CAN-FIND(FIRST KjedensButikker OF DriftsType) THEN
      DO:
        DELETE DriftsType NO-ERROR.
        DELETE TT_DriftsType.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_DriftsType WHERE
    TT_DriftsType.RecType = 1:
    FIND DriftsType EXCLUSIVE-LOCK WHERE
      DriftsType.DriftsFormId = TT_DriftsType.DriftsFormId AND
      DriftsType.DriftsTypeId = TT_DriftsType.DriftsTypeId NO-ERROR.
    IF NOT AVAILABLE DriftsType THEN
      CREATE DriftsType.
    BUFFER-COPY TT_DriftsType TO DriftsType.
    RELEASE DriftsType.
    DELETE TT_DriftsType.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdFarg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdFarg Procedure 
PROCEDURE oppdFarg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Farg) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF Farg OVERRIDE 
  DO:  
  END.
  ON WRITE OF Farg OVERRIDE 
  DO:  
  END.
  ON DELETE OF Farg OVERRIDE 
  DO:  
  END.
  */

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Farg WHERE
    TT_Farg.RecType = 3:
    FIND Farg EXCLUSIVE-LOCK WHERE
      Farg.Farg = TT_Farg.Farg NO-ERROR.
    IF AVAILABLE Farg THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF Farg) THEN
      DO:
        DELETE Farg NO-ERROR.
        DELETE TT_Farg.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Farg WHERE
    TT_Farg.RecType = 1:
    FIND Farg EXCLUSIVE-LOCK WHERE
      Farg.Farg = TT_Farg.Farg NO-ERROR.
    IF NOT AVAILABLE Farg THEN
      CREATE Farg.
    BUFFER-COPY TT_Farg TO Farg.
    RELEASE Farg.
    DELETE TT_Farg.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdFeilkode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdFeilkode Procedure 
PROCEDURE oppdFeilkode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Feilkode) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF Feilkode OVERRIDE 
  DO:  
  END.
  ON WRITE OF Feilkode OVERRIDE 
  DO:  
  END.
  ON DELETE OF Feilkode OVERRIDE 
  DO:  
  END.
  */
  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Feilkode WHERE
    TT_Feilkode.RecType = 3:
    FIND Feilkode EXCLUSIVE-LOCK WHERE
      Feilkode.Feilkode = TT_Feilkode.Feilkode NO-ERROR.
    IF AVAILABLE Feilkode THEN
    DO:
      /*IF NOT CAN-FIND(FIRST VgKat WHERE VgKat.KatNr = Feilkode.KatNr) THEN*/
      DO:
        DELETE Feilkode NO-ERROR.
        DELETE TT_Feilkode.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Feilkode WHERE
    TT_Feilkode.RecType = 1:
    FIND Feilkode EXCLUSIVE-LOCK WHERE
      Feilkode.Feilkode = TT_Feilkode.Feilkode NO-ERROR.
    IF NOT AVAILABLE Feilkode THEN
      CREATE Feilkode.
    BUFFER-COPY TT_Feilkode TO Feilkode.
    RELEASE Feilkode.
    DELETE TT_Feilkode.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdFoder) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdFoder Procedure 
PROCEDURE oppdFoder :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Foder) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  ON CREATE OF Foder OVERRIDE 
  DO:  
  END.
  ON WRITE OF Foder OVERRIDE 
  DO:  
  END.
  ON DELETE OF Foder OVERRIDE 
  DO:  
  END.
  
  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Foder WHERE
    TT_Foder.RecType = 3:
    FIND Foder EXCLUSIVE-LOCK WHERE
      Foder.Foder-Id = TT_Foder.Foder-Id NO-ERROR.
    IF AVAILABLE Foder THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF Foder) THEN
      DO:
        DELETE Foder NO-ERROR.
        DELETE TT_Foder.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Foder WHERE
    TT_Foder.RecType = 1:
    FIND Foder EXCLUSIVE-LOCK WHERE
      Foder.Foder-Id = TT_Foder.Foder-Id NO-ERROR.
    IF NOT AVAILABLE Foder THEN
      CREATE Foder.
    BUFFER-COPY TT_Foder TO Foder.
    RELEASE Foder.
    DELETE TT_Foder.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdGaranti) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdGaranti Procedure 
PROCEDURE oppdGaranti :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Garanti) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF Garanti OVERRIDE 
  DO:  
  END.
  ON WRITE OF Garanti OVERRIDE 
  DO:  
  END.
  ON DELETE OF Garanti OVERRIDE 
  DO:  
  END.
  */

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Garanti WHERE
    TT_Garanti.RecType = 3:
    FIND Garanti EXCLUSIVE-LOCK WHERE
      Garanti.garantikl = TT_Garanti.garantikl NO-ERROR.
    IF AVAILABLE Garanti THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF Garanti) THEN
      DO:
        DELETE Garanti NO-ERROR.
        DELETE TT_Garanti.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Garanti WHERE
    TT_Garanti.RecType = 1:
    FIND Garanti EXCLUSIVE-LOCK WHERE
      Garanti.garantikl = TT_Garanti.garantikl NO-ERROR.
    IF NOT AVAILABLE Garanti THEN
      CREATE Garanti.
    BUFFER-COPY TT_Garanti TO Garanti.
    RELEASE Garanti.
    DELETE TT_Garanti.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdGaveKType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdGaveKType Procedure 
PROCEDURE oppdGaveKType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_GaveKType) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF GaveKType OVERRIDE 
  DO:  
  END.
  ON WRITE OF GaveKType OVERRIDE 
  DO:  
  END.
  ON DELETE OF GaveKType OVERRIDE 
  DO:  
  END.
  */

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_GaveKType WHERE
    TT_GaveKType.RecType = 3:
    FIND GaveKType EXCLUSIVE-LOCK WHERE
      GaveKType.IdentType = TT_GaveKType.IdentType NO-ERROR.
    IF AVAILABLE GaveKType THEN
    DO:
      IF NOT CAN-FIND(FIRST Gavekort OF GaveKType) THEN
      DO:
        DELETE GaveKType NO-ERROR.
        DELETE TT_GaveKType.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_GaveKType WHERE
    TT_GaveKType.RecType = 1:
    FIND GaveKType EXCLUSIVE-LOCK WHERE
      GaveKType.IdentType = TT_GaveKType.IdentType NO-ERROR.
    IF NOT AVAILABLE GaveKType THEN
      CREATE GaveKType.
    BUFFER-COPY TT_GaveKType TO GaveKType.
    RELEASE GaveKType.
    DELETE TT_GaveKType.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdHandtering) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdHandtering Procedure 
PROCEDURE oppdHandtering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Handtering) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  ON CREATE OF Handtering OVERRIDE 
  DO:  
  END.
  ON WRITE OF Handtering OVERRIDE 
  DO:  
  END.
  ON DELETE OF Handtering OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Handtering WHERE
    TT_Handtering.RecType = 3:
    FIND Handtering EXCLUSIVE-LOCK WHERE
      Handtering.HandKode = TT_Handtering.HandKode NO-ERROR.
    IF AVAILABLE Handtering THEN
    DO:
      /*IF NOT CAN-FIND(FIRST VgKat WHERE VgKat.KatNr = Handtering.KatNr) THEN*/
      DO:
        DELETE Handtering NO-ERROR.
        DELETE TT_Handtering.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Handtering WHERE
    TT_Handtering.RecType = 1:
    FIND Handtering EXCLUSIVE-LOCK WHERE
      Handtering.HandKode = TT_Handtering.HandKode NO-ERROR.
    IF NOT AVAILABLE Handtering THEN
      CREATE Handtering.
    BUFFER-COPY TT_Handtering TO Handtering.
    RELEASE Handtering.
    DELETE TT_Handtering.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdHuvGr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdHuvGr Procedure 
PROCEDURE oppdHuvGr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_HuvGr) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  ON CREATE OF HuvGr OVERRIDE 
  DO:  
  END.
  ON WRITE OF HuvGr OVERRIDE 
  DO:  
  END.
  ON DELETE OF HuvGr OVERRIDE 
  DO:  
  END.
  
  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_HuvGr WHERE
    TT_HuvGr.RecType = 3:
    FIND HuvGr EXCLUSIVE-LOCK WHERE
      HuvGr.Hg = TT_HuvGr.Hg NO-ERROR.
    IF AVAILABLE HuvGr THEN
    DO:
      IF NOT CAN-FIND(FIRST VarGr OF HuvGr) THEN
      DO:
        DELETE HuvGr NO-ERROR.
        DELETE TT_HuvGr.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_HuvGr WHERE
    TT_HuvGr.RecType = 1:
    FIND HuvGr EXCLUSIVE-LOCK WHERE
      HuvGr.Hg = TT_HuvGr.Hg NO-ERROR.
    IF NOT AVAILABLE HuvGr THEN
      CREATE HuvGr.
    BUFFER-COPY TT_HuvGr TO HuvGr.
    RELEASE HuvGr.
    DELETE TT_HuvGr.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdIndType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdIndType Procedure 
PROCEDURE oppdIndType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Indtype) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF Indtype OVERRIDE 
  DO:  
  END.
  ON WRITE OF Indtype OVERRIDE 
  DO:  
  END.
  ON DELETE OF Indtype OVERRIDE 
  DO:  
  END.
  */

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Indtype WHERE
    TT_Indtype.RecType = 3:
    FIND Indtype EXCLUSIVE-LOCK WHERE
      Indtype.IndividType = TT_Indtype.IndividType NO-ERROR.
    IF AVAILABLE Indtype THEN
    DO:
      IF NOT CAN-FIND(FIRST Individ OF Indtype) THEN
      DO:
        DELETE Indtype NO-ERROR.
        DELETE TT_Indtype.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Indtype WHERE
    TT_Indtype.RecType = 1:
    FIND Indtype EXCLUSIVE-LOCK WHERE
      Indtype.IndividType = TT_Indtype.IndividType NO-ERROR.
    IF NOT AVAILABLE Indtype THEN
      CREATE Indtype.
    BUFFER-COPY TT_Indtype TO Indtype.
    RELEASE Indtype.
    DELETE TT_Indtype.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdInnBetType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdInnBetType Procedure 
PROCEDURE oppdInnBetType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_InnBetType) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF InnBetType OVERRIDE 
  DO:  
  END.
  ON WRITE OF InnBetType OVERRIDE 
  DO:  
  END.
  ON DELETE OF InnBetType OVERRIDE 
  DO:  
  END.
  */

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_InnBetType WHERE
    TT_InnBetType.RecType = 3:
    FIND InnBetType EXCLUSIVE-LOCK WHERE
      InnBetType.InnBetTId = TT_InnBetType.InnBetTId NO-ERROR.
    IF AVAILABLE InnBetType THEN
    DO:
      /*IF NOT CAN-FIND(FIRST ArtBas OF InnBetType) THEN*/
      DO:
        DELETE InnBetType NO-ERROR.
        DELETE TT_InnBetType.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_InnBetType WHERE
    TT_InnBetType.RecType = 1:
    FIND InnBetType EXCLUSIVE-LOCK WHERE
      InnBetType.InnBetTId = TT_InnBetType.InnBetTId NO-ERROR.
    IF NOT AVAILABLE InnBetType THEN
      CREATE InnBetType.
    BUFFER-COPY TT_InnBetType TO InnBetType.
    RELEASE InnBetType.
    DELETE TT_InnBetType.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdInnersula) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdInnersula Procedure 
PROCEDURE oppdInnersula :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Innersula) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF Innersula OVERRIDE 
  DO:  
  END.
  ON WRITE OF Innersula OVERRIDE 
  DO:  
  END.
  ON DELETE OF Innersula OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Innersula WHERE
    TT_Innersula.RecType = 3:
    FIND Innersula EXCLUSIVE-LOCK WHERE
      Innersula.Inner-Id = TT_Innersula.Inner-Id NO-ERROR.
    IF AVAILABLE Innersula THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF Innersula) THEN
      DO:
        DELETE Innersula NO-ERROR.
        DELETE TT_Innersula.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Innersula WHERE
    TT_Innersula.RecType = 1:
    FIND Innersula EXCLUSIVE-LOCK WHERE
      Innersula.Inner-Id = TT_Innersula.Inner-Id NO-ERROR.
    IF NOT AVAILABLE Innersula THEN
      CREATE Innersula.
    BUFFER-COPY TT_Innersula TO Innersula.
    RELEASE Innersula.
    DELETE TT_Innersula.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdKasValuta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdKasValuta Procedure 
PROCEDURE oppdKasValuta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_KasValuta) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF KasValuta OVERRIDE 
  DO:  
  END.
  ON WRITE OF KasValuta OVERRIDE 
  DO:  
  END.
  ON DELETE OF KasValuta OVERRIDE 
  DO:  
  END.
  */

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_KasValuta WHERE
    TT_KasValuta.RecType = 3:
    FIND KasValuta EXCLUSIVE-LOCK WHERE
      KasValuta.ValKod = TT_KasValuta.ValKod NO-ERROR.
    IF AVAILABLE KasValuta THEN
    DO:
      DELETE KasValuta NO-ERROR.
      DELETE TT_KasValuta.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_KasValuta WHERE
    TT_KasValuta.RecType = 1:
    FIND KasValuta EXCLUSIVE-LOCK WHERE
      KasValuta.ValKod = TT_KasValuta.ValKod NO-ERROR.
    IF NOT AVAILABLE KasValuta THEN
      CREATE KasValuta.
    BUFFER-COPY TT_KasValuta TO KasValuta.
    RELEASE KasValuta.
    DELETE TT_KasValuta.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdKategori) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdKategori Procedure 
PROCEDURE oppdKategori :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Kategori) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  ON CREATE OF Kategori OVERRIDE 
  DO:  
  END.
  ON WRITE OF Kategori OVERRIDE 
  DO:  
  END.
  ON DELETE OF Kategori OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Kategori WHERE
    TT_Kategori.RecType = 3:
    FIND Kategori EXCLUSIVE-LOCK WHERE
      Kategori.KatNr = TT_Kategori.KatNr NO-ERROR.
    IF AVAILABLE Kategori THEN
    DO:
      IF NOT CAN-FIND(FIRST VgKat WHERE VgKat.KatNr = Kategori.KatNr) THEN
      DO:
        DELETE Kategori NO-ERROR.
        DELETE TT_Kategori.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Kategori WHERE
    TT_Kategori.RecType = 1:
    FIND Kategori EXCLUSIVE-LOCK WHERE
      Kategori.KatNr = TT_Kategori.KatNr NO-ERROR.
    IF NOT AVAILABLE Kategori THEN
      CREATE Kategori.
    BUFFER-COPY TT_Kategori TO Kategori.
    RELEASE Kategori.
    DELETE TT_Kategori.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdKjede) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdKjede Procedure 
PROCEDURE oppdKjede :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Kjede) THEN
    RETURN.
  ON CREATE OF Kjede OVERRIDE 
  DO:  
  END.
  ON WRITE OF Kjede OVERRIDE 
  DO:  
  END.
  ON DELETE OF Kjede OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Kjede WHERE
    TT_Kjede.RecType = 3:
    FIND Kjede EXCLUSIVE-LOCK WHERE
      Kjede.KjedeNr = TT_Kjede.KjedeNr NO-ERROR.
    IF AVAILABLE Kjede THEN
    DO:
      IF NOT CAN-FIND(FIRST KjedeDistrikt OF Kjede) AND
         NOT CAN-FIND(FIRST KjedensButikker OF Kjede) THEN
      DO:
        DELETE Kjede NO-ERROR.
        DELETE TT_Kjede.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Kjede WHERE
    TT_Kjede.RecType = 1:
    FIND Kjede EXCLUSIVE-LOCK WHERE
      Kjede.KjedeNr = TT_Kjede.KjedeNr NO-ERROR.
    IF NOT AVAILABLE Kjede THEN
      CREATE Kjede.
    BUFFER-COPY TT_Kjede TO Kjede.
    RELEASE Kjede.
    DELETE TT_Kjede.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdKjedeDistrikt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdKjedeDistrikt Procedure 
PROCEDURE oppdKjedeDistrikt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_KjedeDistrikt) THEN
    RETURN.
  /* Kobler ut logging. */
  ON CREATE OF KjedeDistrikt OVERRIDE 
  DO:  
  END.
  ON WRITE OF KjedeDistrikt OVERRIDE 
  DO:  
  END.
  ON DELETE OF KjedeDistrikt OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_KjedeDistrikt WHERE
    TT_KjedeDistrikt.RecType = 3:
    FIND KjedeDistrikt EXCLUSIVE-LOCK WHERE
      KjedeDistrikt.KjedeNr       = TT_KjedeDistrikt.KjedeNr AND
      KjedeDistrikt.RegionNr      = TT_KjedeDistrikt.RegionNr AND
      KjedeDistrikt.DistriktNr    = TT_KjedeDistrikt.DistriktNr NO-ERROR.
    IF AVAILABLE KjedeDistrikt THEN
    DO:
      IF NOT CAN-FIND(FIRST KjedensButikker OF KjedeDistrikt) THEN
      DO:
        DELETE KjedeDistrikt NO-ERROR.
        DELETE TT_KjedeDistrikt.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_KjedeDistrikt WHERE
    TT_KjedeDistrikt.RecType = 1:
    FIND KjedeDistrikt EXCLUSIVE-LOCK WHERE
      KjedeDistrikt.KjedeNr       = TT_KjedeDistrikt.KjedeNr AND
      KjedeDistrikt.RegionNr      = TT_KjedeDistrikt.RegionNr AND
      KjedeDistrikt.DistriktNr = TT_KjedeDistrikt.DistriktNr NO-ERROR.
    IF NOT AVAILABLE KjedeDistrikt THEN
      CREATE KjedeDistrikt.
    BUFFER-COPY TT_KjedeDistrikt TO KjedeDistrikt.
    RELEASE KjedeDistrikt.
    DELETE TT_KjedeDistrikt.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdKjedensButikker) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdKjedensButikker Procedure 
PROCEDURE oppdKjedensButikker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_KjedensButikker) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  ON CREATE OF KjedensButikker OVERRIDE 
  DO:  
  END.
  ON WRITE OF KjedensButikker OVERRIDE 
  DO:  
  END.
  ON DELETE OF KjedensButikker OVERRIDE 
  DO:  
  END.
  
  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_KjedensButikker WHERE
    TT_KjedensButikker.RecType = 3:
    FIND KjedensButikker EXCLUSIVE-LOCK WHERE
      KjedensButikker.ButikkNr = TT_KjedensButikker.ButikkNr NO-ERROR.
    IF AVAILABLE KjedensButikker THEN
    DO:
      DO:
        DELETE KjedensButikker NO-ERROR.
        DELETE TT_KjedensButikker.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_KjedensButikker WHERE
    TT_KjedensButikker.RecType = 1:
    FIND KjedensButikker EXCLUSIVE-LOCK WHERE
      KjedensButikker.ButikkNr = TT_KjedensButikker.ButikkNr NO-ERROR.
    IF NOT AVAILABLE KjedensButikker THEN
      CREATE KjedensButikker.
    BUFFER-COPY TT_KjedensButikker TO KjedensButikker.
    RELEASE KjedensButikker.
    DELETE TT_KjedensButikker.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdKjedeRegion) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdKjedeRegion Procedure 
PROCEDURE oppdKjedeRegion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_KjedeRegion) THEN
    RETURN.
  ON CREATE OF KjedeRegion OVERRIDE 
  DO:  
  END.
  ON WRITE OF KjedeRegion OVERRIDE 
  DO:  
  END.
  ON DELETE OF KjedeRegion OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_KjedeRegion WHERE
    TT_KjedeRegion.RecType = 3:
    FIND KjedeRegion EXCLUSIVE-LOCK WHERE
      KjedeRegion.KjedeNr  = TT_KjedeRegion.KjedeNr AND
      KjedeRegion.RegionNr = TT_KjedeRegion.RegionNr NO-ERROR.
    IF AVAILABLE KjedeRegion THEN
    DO:
      IF NOT CAN-FIND(FIRST KjedensButikker OF KjedeRegion) THEN
      DO:
        DELETE KjedeRegion NO-ERROR.
        DELETE TT_KjedeRegion.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_KjedeRegion WHERE
    TT_KjedeRegion.RecType = 1:
    FIND KjedeRegion EXCLUSIVE-LOCK WHERE
      KjedeRegion.KjedeNr  = TT_KjedeRegion.KjedeNr AND
      KjedeRegion.RegionNr = TT_KjedeRegion.RegionNr NO-ERROR.
    IF NOT AVAILABLE KjedeRegion THEN
      CREATE KjedeRegion.
    BUFFER-COPY TT_KjedeRegion TO KjedeRegion.
    RELEASE KjedeRegion.
    DELETE TT_KjedeRegion.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdKlack) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdKlack Procedure 
PROCEDURE oppdKlack :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Klack) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF Klack OVERRIDE 
  DO:  
  END.
  ON WRITE OF Klack OVERRIDE 
  DO:  
  END.
  ON DELETE OF Klack OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Klack WHERE
    TT_Klack.RecType = 3:
    FIND Klack EXCLUSIVE-LOCK WHERE
      Klack.Klack-Id = TT_Klack.Klack-Id NO-ERROR.
    IF AVAILABLE Klack THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas WHERE 
                      ArtBas.Klack = Klack.Klack-Id) THEN
      DO:
        DELETE Klack NO-ERROR.
        DELETE TT_Klack.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Klack WHERE
    TT_Klack.RecType = 1:
    FIND Klack EXCLUSIVE-LOCK WHERE
      Klack.Klack-Id = TT_Klack.Klack-Id NO-ERROR.
    IF NOT AVAILABLE Klack THEN
      CREATE Klack.
    BUFFER-COPY TT_Klack TO Klack.
    RELEASE Klack.
    DELETE TT_Klack.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdKravkode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdKravkode Procedure 
PROCEDURE oppdKravkode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Kravkode) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF Kravkode OVERRIDE 
  DO:  
  END.
  ON WRITE OF Kravkode OVERRIDE 
  DO:  
  END.
  ON DELETE OF Kravkode OVERRIDE 
  DO:  
  END.
  */
  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Kravkode WHERE
    TT_Kravkode.RecType = 3:
    FIND Kravkode EXCLUSIVE-LOCK WHERE
      Kravkode.Kravkode = TT_Kravkode.Kravkode NO-ERROR.
    IF AVAILABLE Kravkode THEN
    DO:
      /*IF NOT CAN-FIND(FIRST ArtBas WHERE 
                      ArtBas.Kravkode = Kravkode.Kravkode-Id) THEN*/
      DO:
        DELETE Kravkode NO-ERROR.
        DELETE TT_Kravkode.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Kravkode WHERE
    TT_Kravkode.RecType = 1:
    FIND Kravkode EXCLUSIVE-LOCK WHERE
      Kravkode.Kravkode = TT_Kravkode.Kravkode NO-ERROR.
    IF NOT AVAILABLE Kravkode THEN
      CREATE Kravkode.
    BUFFER-COPY TT_Kravkode TO Kravkode.
    RELEASE Kravkode.
    DELETE TT_Kravkode.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdLast-Sko) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdLast-Sko Procedure 
PROCEDURE oppdLast-Sko :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Last-Sko) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF Last-Sko OVERRIDE 
  DO:  
  END.
  ON WRITE OF Last-Sko OVERRIDE 
  DO:  
  END.
  ON DELETE OF Last-Sko OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Last-Sko WHERE
    TT_Last-Sko.RecType = 3:
    FIND Last-Sko EXCLUSIVE-LOCK WHERE
      Last-Sko.Last-Id = TT_Last-Sko.Last-Id NO-ERROR.
    IF AVAILABLE Last-Sko THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF Last-Sko) THEN
      DO:
        DELETE Last-Sko NO-ERROR.
        DELETE TT_Last-Sko.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Last-Sko WHERE
    TT_Last-Sko.RecType = 1:
    FIND Last-Sko EXCLUSIVE-LOCK WHERE
      Last-Sko.Last-Id = TT_Last-Sko.Last-Id NO-ERROR.
    IF NOT AVAILABLE Last-Sko THEN
      CREATE Last-Sko.
    BUFFER-COPY TT_Last-Sko TO Last-Sko.
    RELEASE Last-Sko.
    DELETE TT_Last-Sko.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdLevKontakt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdLevKontakt Procedure 
PROCEDURE oppdLevKontakt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* TN 4/9-09
  IF NOT CAN-FIND(FIRST TT_LevKontakt) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF LevKontakt OVERRIDE 
  DO:  
  END.
  ON WRITE OF LevKontakt OVERRIDE 
  DO:  
  END.
  ON DELETE OF LevKontakt OVERRIDE 
  DO:  
  END.
  */

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_LevKontakt WHERE
    TT_LevKontakt.RecType = 3:
    FIND LevKontakt EXCLUSIVE-LOCK WHERE
      LevKontakt.LevNr  = TT_LevKontakt.LevNr AND
      LevKontakt.KontNr = TT_LevKontakt.KontNr NO-ERROR.
    IF AVAILABLE LevKontakt THEN
    DO:
        DELETE LevKontakt NO-ERROR.
        DELETE TT_LevKontakt.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_LevKontakt WHERE
    TT_LevKontakt.RecType = 1:
    FIND LevKontakt EXCLUSIVE-LOCK WHERE
      LevKontakt.LevNr  = TT_LevKontakt.LevNr AND
      LevKontakt.KontNr = TT_LevKontakt.KontNr NO-ERROR.
    IF NOT AVAILABLE LevKontakt THEN
      CREATE LevKontakt.
    BUFFER-COPY TT_LevKontakt TO LevKontakt.
    RELEASE LevKontakt.
    DELETE TT_LevKontakt.
  END. /* NY-ENDRE */
*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdLevSort) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdLevSort Procedure 
PROCEDURE oppdLevSort :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_LevSort) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF LevSort OVERRIDE 
  DO:  
  END.
  ON WRITE OF LevSort OVERRIDE 
  DO:  
  END.
  ON DELETE OF LevSort OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_LevSort WHERE
    TT_LevSort.RecType = 3:
    FIND LevSort EXCLUSIVE-LOCK WHERE
      LevSort.LevNr  = TT_LevSort.LevNr AND
      LevSort.SortId = TT_LevSort.SortId NO-ERROR.
    IF AVAILABLE LevSort THEN
    DO:
      IF NOT CAN-FIND(FIRST LevBas OF LevSort) THEN
      DO:
        FOR EACH LevSAnt OF LevSort:
          DELETE LevSAnt.
        END.
        DELETE LevSort NO-ERROR.
        DELETE TT_LevSort.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_LevSort WHERE
    TT_LevSort.RecType = 1:
    FIND LevSort EXCLUSIVE-LOCK WHERE
      LevSort.LevNr  = TT_LevSort.LevNr AND
      LevSort.SortId = TT_LevSort.SortId NO-ERROR.
    IF NOT AVAILABLE LevSort THEN
      CREATE LevSort.
    ELSE DO:
      /* Ved endring tas alle størrelser bort oglegges opp pånytt */
      FOR EACH LevSAnt WHERE 
        LevSAnt.LevNr  = LevSort.LevNr AND
        LevSAnt.SortId = LevSort.SortId:
        DELETE LevSAnt.
      END.
    END.
    BUFFER-COPY TT_LevSort TO LevSort.
    /* Legger opp størrelsene */
    FOR EACH TT_LevSAnt WHERE
      TT_LevSAnt.LevNr  = TT_LevSort.LevNr AND
      TT_LevSAnt.SortId = TT_LevSort.SortId:
      CREATE LevSAnt.
      BUFFER-COPY TT_LevSAnt TO LevSAnt.
    END.
    IF AVAILABLE LevSAnt THEN
      RELEASE LevSAnt.
    RELEASE LevSort.
    DELETE TT_LevSort.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdLokalGruppering) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdLokalGruppering Procedure 
PROCEDURE oppdLokalGruppering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_LokalGruppering) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF LokalGruppering OVERRIDE 
  DO:  
  END.
  ON WRITE OF LokalGruppering OVERRIDE 
  DO:  
  END.
  ON DELETE OF LokalGruppering OVERRIDE 
  DO:  
  END.
  */

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_LokalGruppering WHERE
    TT_LokalGruppering.RecType = 3:
    FIND LokalGruppering EXCLUSIVE-LOCK WHERE
      LokalGruppering.LGId = TT_LokalGruppering.LGId NO-ERROR.
    IF AVAILABLE LokalGruppering THEN
    DO:
      IF NOT CAN-FIND(FIRST KjedensButikker WHERE
                            KjedensButikker.LgId = LokalGruppering.LgId) THEN
      DO:
        DELETE LokalGruppering NO-ERROR.
        DELETE TT_LokalGruppering.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_LokalGruppering WHERE
    TT_LokalGruppering.RecType = 1:
    FIND LokalGruppering EXCLUSIVE-LOCK WHERE
      LokalGruppering.LGId = TT_LokalGruppering.LGId NO-ERROR.
    IF NOT AVAILABLE LokalGruppering THEN
      CREATE LokalGruppering.
    BUFFER-COPY TT_LokalGruppering TO LokalGruppering.
    RELEASE LokalGruppering.
    DELETE TT_LokalGruppering.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdMaterial) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdMaterial Procedure 
PROCEDURE oppdMaterial :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Material) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF Material OVERRIDE 
  DO:  
  END.
  ON WRITE OF Material OVERRIDE 
  DO:  
  END.
  ON DELETE OF Material OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Material WHERE
    TT_Material.RecType = 3:
    FIND Material EXCLUSIVE-LOCK WHERE
      Material.MatKod = TT_Material.MatKod NO-ERROR.
    IF AVAILABLE Material THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF Material) THEN
      DO:
        DELETE Material NO-ERROR.
        DELETE TT_Material.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Material WHERE
    TT_Material.RecType = 1:
    FIND Material EXCLUSIVE-LOCK WHERE
      Material.MatKod = TT_Material.MatKod NO-ERROR.
    IF NOT AVAILABLE Material THEN
      CREATE Material.
    BUFFER-COPY TT_Material TO Material.
    RELEASE Material.
    DELETE TT_Material.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdMesse) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdMesse Procedure 
PROCEDURE oppdMesse :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Messe) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF Messe OVERRIDE 
  DO:  
  END.
  ON WRITE OF Messe OVERRIDE 
  DO:  
  END.
  ON DELETE OF Messe OVERRIDE 
  DO:  
  END.
  */

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Messe WHERE
    TT_Messe.RecType = 3:
    FIND Messe EXCLUSIVE-LOCK WHERE
      Messe.MesseNr = TT_Messe.MesseNr NO-ERROR.
    IF AVAILABLE Messe THEN
    DO:
      IF NOT CAN-FIND(FIRST VareBokHode OF Messe) 
         OR NOT CAN-FIND(FIRST VareBehHode OF Messe) THEN
      DO:
        DELETE Messe NO-ERROR.
        DELETE TT_Messe.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Messe WHERE
    TT_Messe.RecType = 1:
    FIND Messe EXCLUSIVE-LOCK WHERE
      Messe.MesseNr = TT_Messe.MesseNr NO-ERROR.
    IF NOT AVAILABLE Messe THEN
      CREATE Messe.
    BUFFER-COPY TT_Messe TO Messe.
    RELEASE Messe.
    DELETE TT_Messe.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdMoms) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdMoms Procedure 
PROCEDURE oppdMoms :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Moms) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  ON CREATE OF Moms OVERRIDE 
  DO:  
  END.
  ON WRITE OF Moms OVERRIDE 
  DO:  
  END.
  ON DELETE OF Moms OVERRIDE 
  DO:  
  END.
  
  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Moms WHERE
    TT_Moms.RecType = 3:
    FIND Moms EXCLUSIVE-LOCK WHERE
      Moms.MomsKod = TT_Moms.MomsKod NO-ERROR.
    IF AVAILABLE Moms THEN
    DO:
      IF NOT CAN-FIND(FIRST VarGr OF Moms) THEN
      DO:
        DELETE Moms NO-ERROR.
        DELETE TT_Moms.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Moms WHERE
    TT_Moms.RecType = 1:
    FIND Moms EXCLUSIVE-LOCK WHERE
      Moms.MomsKod = TT_Moms.MomsKod NO-ERROR.
    IF NOT AVAILABLE Moms THEN
      CREATE Moms.
    BUFFER-COPY TT_Moms TO Moms.
    RELEASE Moms.
    DELETE TT_Moms.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdOvandel) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdOvandel Procedure 
PROCEDURE oppdOvandel :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Ovandel) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  ON CREATE OF Ovandel OVERRIDE 
  DO:  
  END.
  ON WRITE OF Ovandel OVERRIDE 
  DO:  
  END.
  ON DELETE OF Ovandel OVERRIDE 
  DO:  
  END.
  
  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Ovandel WHERE
    TT_Ovandel.RecType = 3:
    FIND Ovandel EXCLUSIVE-LOCK WHERE
      Ovandel.Ov-Id = TT_Ovandel.Ov-Id NO-ERROR.
    IF AVAILABLE Ovandel THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF Ovandel) THEN
      DO:
        DELETE Ovandel NO-ERROR.
        DELETE TT_Ovandel.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Ovandel WHERE
    TT_Ovandel.RecType = 1:
    FIND Ovandel EXCLUSIVE-LOCK WHERE
      Ovandel.Ov-Id = TT_Ovandel.Ov-Id NO-ERROR.
    IF NOT AVAILABLE Ovandel THEN
      CREATE Ovandel.
    BUFFER-COPY TT_Ovandel TO Ovandel.
    RELEASE Ovandel.
    DELETE TT_Ovandel.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdPost) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdPost Procedure 
PROCEDURE oppdPost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Post) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  ON CREATE OF Post OVERRIDE 
  DO:  
  END.
  ON WRITE OF Post OVERRIDE 
  DO:  
  END.
  ON DELETE OF Post OVERRIDE 
  DO:  
  END.
  
  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Post WHERE
    TT_Post.RecType = 3:
    FIND Post EXCLUSIVE-LOCK WHERE
      Post.PostNr = TT_Post.PostNr NO-ERROR.
    IF AVAILABLE Post THEN
    DO:
      IF NOT CAN-FIND(FIRST LevBas WHERE LevBas.LevPoNr = Post.PostNr) AND 
         NOT CAN-FIND(FIRST Kunde  WHERE Kunde.PostNr   = Post.PostNr) AND 
         NOT CAN-FIND(FIRST Medlem WHERE Medlem.PostNr  = Post.PostNr) AND 
         NOT CAN-FIND(FIRST Butiker WHERE Butiker.BuPoNr = Post.PostNr) THEN
      DO:
        DELETE Post NO-ERROR.
        DELETE TT_Post.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Post WHERE
    TT_Post.RecType = 1:
    FIND Post EXCLUSIVE-LOCK WHERE
      Post.PostNr = TT_Post.PostNr NO-ERROR.
    IF NOT AVAILABLE Post THEN
      CREATE Post.
    BUFFER-COPY TT_Post TO Post.
    RELEASE Post.
    DELETE TT_Post.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdPrisgruppe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdPrisgruppe Procedure 
PROCEDURE oppdPrisgruppe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Prisgruppe) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF Prisgruppe OVERRIDE 
  DO:  
  END.
  ON WRITE OF Prisgruppe OVERRIDE 
  DO:  
  END.
  ON DELETE OF Prisgruppe OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Prisgruppe WHERE
    TT_Prisgruppe.RecType = 3:
    FIND Prisgruppe EXCLUSIVE-LOCK WHERE
      Prisgruppe.PrisGrpNr = TT_Prisgruppe.PrisGrpNr NO-ERROR.
    IF AVAILABLE Prisgruppe THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF Prisgruppe) THEN
      DO:
        DELETE Prisgruppe NO-ERROR.
        DELETE TT_Prisgruppe.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Prisgruppe WHERE
    TT_Prisgruppe.RecType = 1:
    FIND Prisgruppe EXCLUSIVE-LOCK WHERE
      Prisgruppe.PrisGrpNr = TT_Prisgruppe.PrisGrpNr NO-ERROR.
    IF NOT AVAILABLE Prisgruppe THEN
      CREATE Prisgruppe.
    BUFFER-COPY TT_Prisgruppe TO Prisgruppe.
    RELEASE Prisgruppe.
    DELETE TT_Prisgruppe.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdPrisprofil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdPrisprofil Procedure 
PROCEDURE oppdPrisprofil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Prisprofil) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF Prisprofil OVERRIDE 
  DO:  
  END.
  ON WRITE OF Prisprofil OVERRIDE 
  DO:  
  END.
  ON DELETE OF Prisprofil OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Prisprofil WHERE
    TT_Prisprofil.RecType = 3:
    FIND Prisprofil EXCLUSIVE-LOCK WHERE
      Prisprofil.ProfilNr = TT_Prisprofil.ProfilNr NO-ERROR.
    IF AVAILABLE Prisprofil THEN
    DO:
      IF NOT CAN-FIND(FIRST Butiker WHERE 
                      Butiker.ProfilNr = Prisprofil.ProfilNr) THEN
      DO:
        DELETE Prisprofil NO-ERROR.
        DELETE TT_Prisprofil.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Prisprofil WHERE
    TT_Prisprofil.RecType = 1:
    FIND Prisprofil EXCLUSIVE-LOCK WHERE
      Prisprofil.ProfilNr = TT_Prisprofil.ProfilNr NO-ERROR.
    IF NOT AVAILABLE Prisprofil THEN
      CREATE Prisprofil.
    BUFFER-COPY TT_Prisprofil TO Prisprofil.
    RELEASE Prisprofil.
    DELETE TT_Prisprofil.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdProdusent) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdProdusent Procedure 
PROCEDURE oppdProdusent :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Produsent) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  ON CREATE OF Produsent OVERRIDE 
  DO:  
  END.
  ON WRITE OF Produsent OVERRIDE 
  DO:  
  END.
  ON DELETE OF Produsent OVERRIDE 
  DO:  
  END.
  
  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Produsent WHERE
    TT_Produsent.RecType = 3:
    FIND Produsent EXCLUSIVE-LOCK WHERE
      Produsent.ProdNr = TT_Produsent.ProdNr NO-ERROR.
    IF AVAILABLE Produsent THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF Produsent) THEN
      DO:
        DELETE Produsent NO-ERROR.
        DELETE TT_Produsent.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Produsent WHERE
    TT_Produsent.RecType = 1:
    FIND Produsent EXCLUSIVE-LOCK WHERE
      Produsent.ProdNr = TT_Produsent.ProdNr NO-ERROR.
    IF NOT AVAILABLE Produsent THEN
      CREATE Produsent.
    BUFFER-COPY TT_Produsent TO Produsent.
    RELEASE Produsent.
    DELETE TT_Produsent.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdProgramListe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdProgramListe Procedure 
PROCEDURE oppdProgramListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_ProgramListe) THEN
    RETURN.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_ProgramListe WHERE
    TT_ProgramListe.RecType = 3:
    FIND ProgramListe EXCLUSIVE-LOCK WHERE
      ProgramListe.ProgNavn = TT_ProgramListe.ProgNavn NO-ERROR.
    IF AVAILABLE ProgramListe THEN
    DO:
      DO:
        /* Tar også bort brukergruppens kobling til programmet. */
        FOR EACH ProgBrGrp WHERE
            ProgBrGrp.ProgNavn = ProgramListe.ProgNavn:
            DELETE ProgBrGrp.
        END.
        DELETE ProgramListe NO-ERROR.
        DELETE TT_ProgramListe.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_ProgramListe WHERE
    TT_ProgramListe.RecType = 1:
    FIND ProgramListe EXCLUSIVE-LOCK WHERE
      ProgramListe.ProgNavn = TT_ProgramListe.ProgNavn NO-ERROR.
    IF NOT AVAILABLE ProgramListe THEN
      CREATE ProgramListe.
    BUFFER-COPY TT_ProgramListe TO ProgramListe.
    RELEASE ProgramListe.
    DELETE TT_ProgramListe.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdProv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdProv Procedure 
PROCEDURE oppdProv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Prov) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  ON CREATE OF Prov OVERRIDE 
  DO:  
  END.
  ON WRITE OF Prov OVERRIDE 
  DO:  
  END.
  ON DELETE OF Prov OVERRIDE 
  DO:  
  END.
  
  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Prov WHERE
    TT_Prov.RecType = 3:
    FIND Prov EXCLUSIVE-LOCK WHERE
      Prov.ProvKod = TT_Prov.ProvKod NO-ERROR.
    IF AVAILABLE Prov THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF Prov) THEN
      DO:
        DELETE Prov NO-ERROR.
        DELETE TT_Prov.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Prov WHERE
    TT_Prov.RecType = 1:
    FIND Prov EXCLUSIVE-LOCK WHERE
      Prov.ProvKod = TT_Prov.ProvKod NO-ERROR.
    IF NOT AVAILABLE Prov THEN
      CREATE Prov.
    BUFFER-COPY TT_Prov TO Prov.
    RELEASE Prov.
    DELETE TT_Prov.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdRabatt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdRabatt Procedure 
PROCEDURE oppdRabatt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Rabatt) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  ON CREATE OF Rabatt OVERRIDE 
  DO:  
  END.
  ON WRITE OF Rabatt OVERRIDE 
  DO:  
  END.
  ON DELETE OF Rabatt OVERRIDE 
  DO:  
  END.
  
  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Rabatt WHERE
    TT_Rabatt.RecType = 3:
    FIND Rabatt EXCLUSIVE-LOCK WHERE
      Rabatt.RabKod = TT_Rabatt.RabKod NO-ERROR.
    IF AVAILABLE Rabatt THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF Rabatt) THEN
      DO:
        DELETE Rabatt NO-ERROR.
        DELETE TT_Rabatt.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Rabatt WHERE
    TT_Rabatt.RecType = 1:
    FIND Rabatt EXCLUSIVE-LOCK WHERE
      Rabatt.RabKod = TT_Rabatt.RabKod NO-ERROR.
    IF NOT AVAILABLE Rabatt THEN
      CREATE Rabatt.
    BUFFER-COPY TT_Rabatt TO Rabatt.
    RELEASE Rabatt.
    DELETE TT_Rabatt.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdSasong) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdSasong Procedure 
PROCEDURE oppdSasong :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Sasong) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  ON CREATE OF Sasong OVERRIDE 
  DO:  
  END.
  ON WRITE OF Sasong OVERRIDE 
  DO:  
  END.
  ON DELETE OF Sasong OVERRIDE 
  DO:  
  END.
  
  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Sasong WHERE
    TT_Sasong.RecType = 3:
    FIND Sasong EXCLUSIVE-LOCK WHERE
      Sasong.Sasong = TT_Sasong.Sasong NO-ERROR.
    IF AVAILABLE Sasong THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF Sasong) THEN
      DO:
        DELETE Sasong NO-ERROR.
        DELETE TT_Sasong.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Sasong WHERE
    TT_Sasong.RecType = 1:
    FIND Sasong EXCLUSIVE-LOCK WHERE
      Sasong.Sasong = TT_Sasong.Sasong NO-ERROR.
    IF NOT AVAILABLE Sasong THEN
      CREATE Sasong.
    BUFFER-COPY TT_Sasong TO Sasong.
    RELEASE Sasong.
    DELETE TT_Sasong.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdSlitsula) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdSlitsula Procedure 
PROCEDURE oppdSlitsula :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Slitsula) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF Slitsula OVERRIDE 
  DO:  
  END.
  ON WRITE OF Slitsula OVERRIDE 
  DO:  
  END.
  ON DELETE OF Slitsula OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Slitsula WHERE
    TT_Slitsula.RecType = 3:
    FIND Slitsula EXCLUSIVE-LOCK WHERE
      Slitsula.Slit-Id = TT_Slitsula.Slit-Id NO-ERROR.
    IF AVAILABLE Slitsula THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF Slitsula) THEN
      DO:
        DELETE Slitsula NO-ERROR.
        DELETE TT_Slitsula.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Slitsula WHERE
    TT_Slitsula.RecType = 1:
    FIND Slitsula EXCLUSIVE-LOCK WHERE
      Slitsula.Slit-Id = TT_Slitsula.Slit-Id NO-ERROR.
    IF NOT AVAILABLE Slitsula THEN
      CREATE Slitsula.
    BUFFER-COPY TT_Slitsula TO Slitsula.
    RELEASE Slitsula.
    DELETE TT_Slitsula.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdStrKonv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdStrKonv Procedure 
PROCEDURE oppdStrKonv :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_StrKonv) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  ON CREATE OF StrKonv OVERRIDE 
  DO:  
  END.
  ON WRITE OF StrKonv OVERRIDE 
  DO:  
  END.
  ON DELETE OF StrKonv OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_StrKonv WHERE
    TT_StrKonv.RecType = 3:
    FIND StrKonv EXCLUSIVE-LOCK WHERE
      StrKonv.StrKode = TT_StrKonv.StrKode NO-ERROR.
    IF AVAILABLE StrKonv THEN
    DO:
      IF NOT CAN-FIND(FIRST Strekkode WHERE
                      Strekkode.StrKode = TT_StrKonv.StrKode) THEN
      DO:
        DELETE StrKonv NO-ERROR.
        DELETE TT_StrKonv.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_StrKonv WHERE
    TT_StrKonv.RecType = 1:
    /* Ordinært oppslag */
    FIND StrKonv EXCLUSIVE-LOCK WHERE
      StrKonv.StrKode = TT_StrKonv.StrKode NO-ERROR.
    IF NOT AVAILABLE StrKonv THEN
      CREATE StrKonv.
    BUFFER-COPY TT_StrKonv TO StrKonv NO-ERROR.
    /* Det har forekommet at at det har ligget lokalt opprettede strkonv.... */
    IF ERROR-STATUS:ERROR AND AVAILABLE StrKonv 
        THEN DELETE StrKonv.
    ELSE
        RELEASE StrKonv.
    DELETE TT_StrKonv.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdStrType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdStrType Procedure 
PROCEDURE oppdStrType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE iAntLinjer AS INTEGER NO-UNDO.
  IF NOT CAN-FIND(FIRST TT_StrType) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF StrType OVERRIDE 
  DO:  
  END.
  ON WRITE OF StrType OVERRIDE 
  DO:  
  END.
  ON DELETE OF StrType OVERRIDE 
  DO:  
  END.
  
  iAntLinjer = 0.
  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_StrType WHERE
    TT_StrType.RecType = 3:
    FIND StrType EXCLUSIVE-LOCK WHERE
      StrType.StrTypeId = TT_StrType.StrTypeId NO-ERROR.
    IF AVAILABLE StrType THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF StrType) THEN
      DO:
        FOR EACH StrTStr OF StrType:
          DELETE StrTStr.
        END.
        DELETE StrType NO-ERROR.
        DELETE TT_StrType.
        iAntLinjer = iAntLinjer + 1.
        STATUS DEFAULT "Sletter linjer " + 
                   STRING(iAntLinjer) + 
                   ".".        
      END.
    END.
  END. /* SLETTEPOSTER */
  
  iAntLinjer = 0.
  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_StrType WHERE
    TT_StrType.RecType = 1:
    FIND StrType EXCLUSIVE-LOCK WHERE
      StrType.StrTypeId = TT_StrType.StrTypeId NO-ERROR.
    IF NOT AVAILABLE StrType THEN
      CREATE StrType.
    ELSE DO:
      /* Ved endring tas alle størrelser bort oglegges opp pånytt */
      FOR EACH StrTStr OF StrType:
        DELETE StrTStr.
      END.
    END.
    BUFFER-COPY TT_StrType TO StrType.
    iAntLinjer = iAntLinjer + 1.
    STATUS DEFAULT "Behandler størrelsestyper. Antall behandlet " + 
               STRING(iAntLinjer) + 
               ".".
    /* Legger opp størrelsene */
    FOR EACH TT_StrTStr WHERE
      TT_StrTStr.StrTypeId = TT_StrType.StrTypeId:
      CREATE StrTStr.
      BUFFER-COPY TT_STrTStr TO StrTStr.
    END.
    IF AVAILABLE StrTStr THEN
      RELEASE STrTStr.
    RELEASE StrType.
    DELETE TT_StrType.
  END. /* NY-ENDRE */

  STATUS DEFAULT " ".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdUtbetType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdUtbetType Procedure 
PROCEDURE oppdUtbetType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_UtbetType) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF UtbetType OVERRIDE 
  DO:  
  END.
  ON WRITE OF UtbetType OVERRIDE 
  DO:  
  END.
  ON DELETE OF UtbetType OVERRIDE 
  DO:  
  END.
  */

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_UtbetType WHERE
    TT_UtbetType.RecType = 3:
    FIND UtbetType EXCLUSIVE-LOCK WHERE
      UtbetType.UtbetTId = TT_UtbetType.UtbetTId NO-ERROR.
    IF AVAILABLE UtbetType THEN
    DO:
      /* Her skal det inn validering på utbetbilag */
      DO:
        DELETE UtbetType NO-ERROR.
        DELETE TT_UtbetType.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_UtbetType WHERE
    TT_UtbetType.RecType = 1:
    FIND UtbetType EXCLUSIVE-LOCK WHERE
      UtbetType.UtbetTId = TT_UtbetType.UtbetTId NO-ERROR.
    IF NOT AVAILABLE UtbetType THEN
      CREATE UtbetType.
    BUFFER-COPY TT_UtbetType TO UtbetType.
    RELEASE UtbetType.
    DELETE TT_UtbetType.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdValuta) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdValuta Procedure 
PROCEDURE oppdValuta :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Valuta) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  ON CREATE OF Valuta OVERRIDE 
  DO:  
  END.
  ON WRITE OF Valuta OVERRIDE 
  DO:  
  END.
  ON DELETE OF Valuta OVERRIDE 
  DO:  
  END.
  
  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Valuta WHERE
    TT_Valuta.RecType = 3:
    FIND Valuta EXCLUSIVE-LOCK WHERE
      Valuta.ValKod = TT_Valuta.ValKod NO-ERROR.
    IF AVAILABLE Valuta THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF Valuta) AND 
         NOT CAN-FIND(FIRST LevBas OF Valuta) THEN
      DO:
        DELETE Valuta NO-ERROR.
        DELETE TT_Valuta.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Valuta WHERE
    TT_Valuta.RecType = 1:
    FIND Valuta EXCLUSIVE-LOCK WHERE
      Valuta.ValKod = TT_Valuta.ValKod NO-ERROR.
    IF NOT AVAILABLE Valuta THEN
      CREATE Valuta.
    BUFFER-COPY TT_Valuta TO Valuta.
    RELEASE Valuta.
    DELETE TT_Valuta.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdVareBehType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdVareBehType Procedure 
PROCEDURE oppdVareBehType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_VareBehType) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF VareBehType OVERRIDE 
  DO:  
  END.
  ON WRITE OF VareBehType OVERRIDE 
  DO:  
  END.
  ON DELETE OF VareBehType OVERRIDE 
  DO:  
  END.
  */

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_VareBehType WHERE
    TT_VareBehType.RecType = 3:
    FIND VareBehType EXCLUSIVE-LOCK WHERE
      VareBehType.VareBehType = TT_VareBehType.VareBehType NO-ERROR.
    IF AVAILABLE VareBehType THEN
    DO:
      IF NOT CAN-FIND(FIRST VareBehHode OF VareBehType) THEN
      DO:
        DELETE VareBehType NO-ERROR.
        DELETE TT_VareBehType.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_VareBehType WHERE
    TT_VareBehType.RecType = 1:
    FIND VareBehType EXCLUSIVE-LOCK WHERE
      VareBehType.VareBehType = TT_VareBehType.VareBehType NO-ERROR.
    IF NOT AVAILABLE VareBehType THEN
      CREATE VareBehType.
    BUFFER-COPY TT_VareBehType TO VareBehType.
    RELEASE VareBehType.
    DELETE TT_VareBehType.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdVareBokType) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdVareBokType Procedure 
PROCEDURE oppdVareBokType :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_VareBokType) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF VareBokType OVERRIDE 
  DO:  
  END.
  ON WRITE OF VareBokType OVERRIDE 
  DO:  
  END.
  ON DELETE OF VareBokType OVERRIDE 
  DO:  
  END.
  */

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_VareBokType WHERE
    TT_VareBokType.RecType = 3:
    FIND VareBokType EXCLUSIVE-LOCK WHERE
      VareBokType.VareBokType = TT_VareBokType.VareBokType NO-ERROR.
    IF AVAILABLE VareBokType THEN
    DO:
      IF NOT CAN-FIND(FIRST VareBokHode OF VareBokType) THEN
      DO:
        DELETE VareBokType NO-ERROR.
        DELETE TT_VareBokType.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_VareBokType WHERE
    TT_VareBokType.RecType = 1:
    FIND VareBokType EXCLUSIVE-LOCK WHERE
      VareBokType.VareBokType = TT_VareBokType.VareBokType NO-ERROR.
    IF NOT AVAILABLE VareBokType THEN
      CREATE VareBokType.
    BUFFER-COPY TT_VareBokType TO VareBokType.
    RELEASE VareBokType.
    DELETE TT_VareBokType.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdVaremerke) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdVaremerke Procedure 
PROCEDURE oppdVaremerke :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_Varemerke) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  ON CREATE OF Varemerke OVERRIDE 
  DO:  
  END.
  ON WRITE OF Varemerke OVERRIDE 
  DO:  
  END.
  ON DELETE OF Varemerke OVERRIDE 
  DO:  
  END.
  
  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_Varemerke WHERE
    TT_Varemerke.RecType = 3:
    FIND Varemerke EXCLUSIVE-LOCK WHERE
      Varemerke.VmId = TT_Varemerke.VmId NO-ERROR.
    IF AVAILABLE Varemerke THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF Varemerke) THEN
      DO:
        DELETE Varemerke NO-ERROR.
        DELETE TT_Varemerke.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_Varemerke WHERE
    TT_Varemerke.RecType = 1:
    FIND Varemerke EXCLUSIVE-LOCK WHERE
      Varemerke.VmId = TT_Varemerke.VmId NO-ERROR.
    IF NOT AVAILABLE Varemerke THEN
      CREATE Varemerke.
    BUFFER-COPY TT_Varemerke TO Varemerke.
    RELEASE Varemerke.
    DELETE TT_Varemerke.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdVarGr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdVarGr Procedure 
PROCEDURE oppdVarGr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_VarGr) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF VarGr OVERRIDE 
  DO:  
  END.
  ON WRITE OF VarGr OVERRIDE 
  DO:  
  END.
  ON DELETE OF VarGr OVERRIDE 
  DO:  
  END.
  */

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_VarGr WHERE
    TT_VarGr.RecType = 3:
    FIND VarGr EXCLUSIVE-LOCK WHERE
      VarGr.Vg = TT_VarGr.Vg NO-ERROR.
    IF AVAILABLE VarGr THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas OF VarGr) THEN
      DO:
        FOR EACH VgKat WHERE
          VgKat.Vg = VarGr.Vg:
          DELETE VgKat.
        END.
        DELETE VarGr NO-ERROR.
        DELETE TT_VarGr.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_VarGr WHERE
    TT_VarGr.RecType = 1:
    FIND VarGr EXCLUSIVE-LOCK WHERE
      VarGr.Vg = TT_VarGr.Vg NO-ERROR.
    IF NOT AVAILABLE VarGr THEN
      CREATE VarGr.
    BUFFER-COPY TT_VarGr TO VarGr.
    /* Oppretter PLU artikkel for varegruppen. */
    IF NEW VarGr THEN
        RUN vgartopris.p (?,VarGr.Vg).
    RELEASE VarGr.
    DELETE TT_VarGr.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdVgAkt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdVgAkt Procedure 
PROCEDURE oppdVgAkt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_VgAkt) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF VgAkt OVERRIDE 
  DO:  
  END.
  ON WRITE OF VgAkt OVERRIDE 
  DO:  
  END.
  ON DELETE OF VgAkt OVERRIDE 
  DO:  
  END.
  */

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_VgAkt WHERE
    TT_VgAkt.RecType = 3:
    FIND VgAkt EXCLUSIVE-LOCK WHERE
      VgAkt.Vg    = TT_VgAkt.Vg AND
      VgAkt.AktNr = TT_VgAkt.AktNr NO-ERROR.
    IF AVAILABLE VgAkt THEN
    DO:
      /*IF NOT CAN-FIND(FIRST VarGr OF VgAkt) THEN*/
      DO:
        DELETE VgAkt NO-ERROR.
        DELETE TT_VgAkt.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_VgAkt WHERE
    TT_VgAkt.RecType = 1:
      FIND VgAkt EXCLUSIVE-LOCK WHERE
        VgAkt.Vg    = TT_VgAkt.Vg AND
        VgAkt.AktNr = TT_VgAkt.AktNr NO-ERROR.
    IF NOT AVAILABLE VgAkt THEN
      CREATE VgAkt.
    BUFFER-COPY TT_VgAkt TO VgAkt.
    RELEASE VgAkt.
    DELETE TT_VgAkt.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdVgKat) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdVgKat Procedure 
PROCEDURE oppdVgKat :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_VgKat) THEN
    RETURN.

  /* Kobler ut logging til kassen. */
  ON CREATE OF VgKat OVERRIDE 
  DO:  
  END.
  ON WRITE OF VgKat OVERRIDE 
  DO:  
  END.
  ON DELETE OF VgKat OVERRIDE 
  DO:  
  END.
  
  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_VgKat WHERE
    TT_VgKat.RecType = 3:
    FIND VgKat EXCLUSIVE-LOCK WHERE
      VgKat.Vg    = TT_VgKat.Vg AND
      VgKat.VgKat = TT_VgKat.VgKat AND
      VgKat.KatNr = TT_VgKat.KatNr
      NO-ERROR.
    IF AVAILABLE VgKat THEN
    DO:
      IF NOT CAN-FIND(FIRST ArtBas WHERE
                      ArtBas.Vg    = VgKat.Vg AND
                      ArtBas.VgKat = VgKat.VgKat) THEN
      DO:
        DELETE VgKat NO-ERROR.
        DELETE TT_VgKat.
      END.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_VgKat WHERE
    TT_VgKat.RecType = 1:
    FIND VgKat EXCLUSIVE-LOCK WHERE
      VgKat.Vg    = TT_VgKat.Vg AND
      VgKat.VgKat = TT_VgKat.VgKat AND
      VgKat.KatNr = TT_VgKat.KatNr
      NO-ERROR.
    IF NOT AVAILABLE VgKat THEN
      CREATE VgKat.
    BUFFER-COPY TT_VgKat TO VgKat.
    RELEASE VgKat.
    DELETE TT_VgKat.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdVPIAltLevBas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdVPIAltLevBas Procedure 
PROCEDURE oppdVPIAltLevBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_VPIAltLevBas) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF VPIAltLevBas OVERRIDE 
  DO:  
  END.
  ON WRITE OF VPIAltLevBas OVERRIDE 
  DO:  
  END.
  ON DELETE OF VPIAltLevBas OVERRIDE 
  DO:  
  END.
  */
  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_VPIAltLevBas WHERE
    TT_VPIAltLevBas.RecType = 3:
    FIND VPIAltLevBas EXCLUSIVE-LOCK WHERE
      VPIAltLevBas.EkstVPILevNr = TT_VPIAltLevBas.EkstVPILevNr AND
      VPIAltLevBas.VareNr       = TT_VPIAltLevBas.VareNr AND
      VPIAltLevBas.LevNr        = TT_VPIAltLevBas.LevNr NO-ERROR.
    IF AVAILABLE VPIAltLevBas THEN
    DO:
      DELETE VPIAltLevBas NO-ERROR.
      DELETE TT_VPIAltLevBas.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_VPIAltLevBas WHERE
    TT_VPIAltLevBas.RecType = 1:
    FIND VPIAltLevBas EXCLUSIVE-LOCK WHERE
      VPIAltLevBas.EkstVPILevNr = TT_VPIAltLevBas.EkstVPILevNr AND
      VPIAltLevBas.VareNr       = TT_VPIAltLevBas.VareNr AND
      VPIAltLevBas.LevNr         = TT_VPIAltLevBas.LevNr NO-ERROR.
    /* Legger inn artikkelinformasjonen */
    IF NOT AVAILABLE VPIAltLevBas THEN
      CREATE VPIAltLevBas.
    BUFFER-COPY TT_VPIAltLevBas TO VPIAltLevBas.
    RELEASE VPIAltLevBas.
    DELETE TT_VPIAltLevBas.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdVPIArtBasBestPkt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdVPIArtBasBestPkt Procedure 
PROCEDURE oppdVPIArtBasBestPkt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* TN 4/9-09
  IF NOT CAN-FIND(FIRST TT_VPIArtBestPkt) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  /*
  ON CREATE OF VPIArtBestPkt OVERRIDE 
  DO:  
  END.
  ON WRITE OF VPIArtBestPkt OVERRIDE 
  DO:  
  END.
  ON DELETE OF VPIArtBestPkt OVERRIDE 
  DO:  
  END.
  */

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_VPIArtBestPkt WHERE
    TT_VPIArtBestPkt.RecType = 3:
    FIND VPIArtBestPkt EXCLUSIVE-LOCK WHERE
      VPIArtBestPkt.EkstVPILevNr = TT_VPIArtBestPkt.EkstVPILevNr AND
      VPIArtBestPkt.VareNr       = TT_VPIArtBestPkt.VareNr AND
      VPIArtBestPkt.StrKode      = TT_VPIArtBestPkt.StrKode NO-ERROR.
    IF AVAILABLE VPIArtBestPkt THEN
    DO:
      DELETE VPIArtBestPkt NO-ERROR.
      DELETE TT_VPIArtBestPkt.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_VPIArtBestPkt WHERE
    TT_VPIArtBestPkt.RecType = 1:
    FIND VPIArtBestPkt EXCLUSIVE-LOCK WHERE
      VPIArtBestPkt.EkstVPILevNr = TT_VPIArtBestPkt.EkstVPILevNr AND
      VPIArtBestPkt.VareNr       = TT_VPIArtBestPkt.VareNr AND
      VPIArtBestPkt.StrKode      = TT_VPIArtBestPkt.StrKode NO-ERROR.
    /* Legger inn artikkelinformasjonen */
    IF NOT AVAILABLE VPIArtBestPkt THEN
      CREATE VPIArtBestPkt.
    BUFFER-COPY TT_VPIArtBestPkt TO VPIArtBestPkt.
    RELEASE VPIArtBestPkt.
    DELETE TT_VPIArtBestPkt.
  END. /* NY-ENDRE */
*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdVPIBildeData) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdVPIBildeData Procedure 
PROCEDURE oppdVPIBildeData :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_VPIBildeData) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF VPIBildeData OVERRIDE 
  DO:  
  END.
  ON WRITE OF VPIBildeData OVERRIDE 
  DO:  
  END.
  ON DELETE OF VPIBildeData OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_VPIBildeData WHERE
    TT_VPIBildeData.RecType = 3:
    FIND VPIBildeData EXCLUSIVE-LOCK WHERE
      VPIBildeData.EkstVPILevNr = TT_VPIBildeData.EkstVPILevNr AND
      VPIBildeData.VareNr       = TT_VPIBildeData.VareNr AND
      VPIBildeData.BildNr       = TT_VPIBildeData.BildNr AND
      VPIBildeData.Teller       = TT_VPIBildeData.Teller
      NO-ERROR.
    IF AVAILABLE VPIBildeData THEN
    DO:
      DELETE VPIBildeData NO-ERROR.
      DELETE TT_VPIBildeData.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_VPIBildeData WHERE
    TT_VPIBildeData.RecType = 1:
    FIND VPIBildeData EXCLUSIVE-LOCK WHERE
        VPIBildeData.EkstVPILevNr = TT_VPIBildeData.EkstVPILevNr AND
        VPIBildeData.VareNr       = TT_VPIBildeData.VareNr AND
        VPIBildeData.BildNr       = TT_VPIBildeData.BildNr AND
        VPIBildeData.Teller       = TT_VPIBildeData.Teller
      NO-ERROR.
    /* Legger inn artikkelinformasjonen */
    IF NOT AVAILABLE VPIBildeData THEN
      CREATE VPIBildeData.
    BUFFER-COPY TT_VPIBildeData TO VPIBildeData.
    RELEASE VPIBildeData.
    DELETE TT_VPIBildeData.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdVPIBildeRegister) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdVPIBildeRegister Procedure 
PROCEDURE oppdVPIBildeRegister :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_VPIBildeRegister) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF VPIBildeRegister OVERRIDE 
  DO:  
  END.
  ON WRITE OF VPIBildeRegister OVERRIDE 
  DO:  
  END.
  ON DELETE OF VPIBildeRegister OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_VPIBildeRegister WHERE
    TT_VPIBildeRegister.RecType = 3:
    FIND VPIBildeRegister EXCLUSIVE-LOCK WHERE
      VPIBildeRegister.EkstVPILevNr = TT_VPIBildeRegister.EkstVPILevNr AND
      VPIBildeRegister.VareNr       = TT_VPIBildeRegister.VareNr AND
      VPIBildeRegister.BildNr       = TT_VPIBildeRegister.BildNr
      NO-ERROR.
    IF AVAILABLE VPIBildeRegister THEN
    DO:
      DELETE VPIBildeRegister NO-ERROR.
      DELETE TT_VPIBildeRegister.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_VPIBildeRegister WHERE
    TT_VPIBildeRegister.RecType = 1:
    FIND VPIBildeRegister EXCLUSIVE-LOCK WHERE
        VPIBildeRegister.EkstVPILevNr = TT_VPIBildeRegister.EkstVPILevNr AND
        VPIBildeRegister.VareNr       = TT_VPIBildeRegister.VareNr AND
        VPIBildeRegister.BildNr       = TT_VPIBildeRegister.BildNr
      NO-ERROR.
    /* Legger inn artikkelinformasjonen */
    IF NOT AVAILABLE VPIBildeRegister THEN
      CREATE VPIBildeRegister.
    BUFFER-COPY TT_VPIBildeRegister TO VPIBildeRegister.
    RELEASE VPIBildeRegister.
    DELETE TT_VPIBildeRegister.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdVPIErstattningsvare) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdVPIErstattningsvare Procedure 
PROCEDURE oppdVPIErstattningsvare :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_VPIErstattningsvare) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF VPIErstattningsvare OVERRIDE 
  DO:  
  END.
  ON WRITE OF VPIErstattningsvare OVERRIDE 
  DO:  
  END.
  ON DELETE OF VPIErstattningsvare OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_VPIErstattningsvare WHERE
    TT_VPIErstattningsvare.RecType = 3:
    FIND VPIErstattningsvare EXCLUSIVE-LOCK WHERE
      VPIErstattningsvare.EkstVPILevNr = TT_VPIErstattningsvare.EkstVPILevNr AND
      VPIErstattningsvare.VareNr       = TT_VPIErstattningsvare.VareNr AND
      VPIErstattningsvare.ArtikkelNr   = TT_VPIErstattningsvare.ArtikkelNr
      NO-ERROR.
    IF AVAILABLE VPIErstattningsvare THEN
    DO:
      DELETE VPIErstattningsvare NO-ERROR.
      DELETE TT_VPIErstattningsvare.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_VPIErstattningsvare WHERE
    TT_VPIErstattningsvare.RecType = 1:
    FIND VPIErstattningsvare EXCLUSIVE-LOCK WHERE
        VPIErstattningsvare.EkstVPILevNr = TT_VPIErstattningsvare.EkstVPILevNr AND
        VPIErstattningsvare.VareNr       = TT_VPIErstattningsvare.VareNr AND
        VPIErstattningsvare.ArtikkelNr   = TT_VPIErstattningsvare.ArtikkelNr
      NO-ERROR.
    /* Legger inn artikkelinformasjonen */
    IF NOT AVAILABLE VPIErstattningsvare THEN
      CREATE VPIErstattningsvare.
    BUFFER-COPY TT_VPIErstattningsvare TO VPIErstattningsvare.
    RELEASE VPIErstattningsvare.
    DELETE TT_VPIErstattningsvare.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdVPIPakkeLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdVPIPakkeLinje Procedure 
PROCEDURE oppdVPIPakkeLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_VPIPakkeLinje) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF VPIPakkeLinje OVERRIDE 
  DO:  
  END.
  ON WRITE OF VPIPakkeLinje OVERRIDE 
  DO:  
  END.
  ON DELETE OF VPIPakkeLinje OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_VPIPakkeLinje WHERE
    TT_VPIPakkeLinje.RecType = 3:
    FIND VPIPakkeLinje EXCLUSIVE-LOCK WHERE
      VPIPakkeLinje.EkstVPILevNr = TT_VPIPakkeLinje.EkstVPILevNr AND
      VPIPakkeLinje.VareNr       = TT_VPIPakkeLinje.VareNr AND
      VPIPakkeLinje.ArtikkelNr   = TT_VPIPakkeLinje.ArtikkelNr AND
      VPIPakkeLinje.PkArtikkelNr = TT_VPIPakkeLinje.PkArtikkelNr AND
      VPIPakkeLinje.StrKode      = TT_VPIPakkeLinje.StrKode
      NO-ERROR.
    IF AVAILABLE VPIPakkeLinje THEN
    DO:
      DELETE VPIPakkeLinje NO-ERROR.
      DELETE TT_VPIPakkeLinje.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_VPIPakkeLinje WHERE
    TT_VPIPakkeLinje.RecType = 1:
    FIND VPIPakkeLinje EXCLUSIVE-LOCK WHERE
        VPIPakkeLinje.EkstVPILevNr = TT_VPIPakkeLinje.EkstVPILevNr AND
        VPIPakkeLinje.VareNr       = TT_VPIPakkeLinje.VareNr AND
        VPIPakkeLinje.ArtikkelNr   = TT_VPIPakkeLinje.ArtikkelNr AND
        VPIPakkeLinje.PkArtikkelNr = TT_VPIPakkeLinje.PkArtikkelNr AND
        VPIPakkeLinje.StrKode      = TT_VPIPakkeLinje.StrKode
      NO-ERROR.
    IF AVAILABLE VPIPakkeLinje THEN
        DELETE VPIPakkeLinje.

    /* Legger inn artikkelinformasjonen */
    IF NOT AVAILABLE VPIPakkeLinje THEN
      CREATE VPIPakkeLinje.
    BUFFER-COPY TT_VPIPakkeLinje TO VPIPakkeLinje.
    RELEASE VPIPakkeLinje.
    DELETE TT_VPIPakkeLinje.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oppdVPIStrekkode) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oppdVPIStrekkode Procedure 
PROCEDURE oppdVPIStrekkode :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF NOT CAN-FIND(FIRST TT_VPIStrekkode) THEN
    RETURN.
  /* Kobler ut logging til kassen. */
  ON CREATE OF VPIStrekkode OVERRIDE 
  DO:  
  END.
  ON WRITE OF VPIStrekkode OVERRIDE 
  DO:  
  END.
  ON DELETE OF VPIStrekkode OVERRIDE 
  DO:  
  END.

  /* Behandler sletteposter */
  SLETTEPOSTER:
  FOR EACH TT_VPIStrekkode WHERE
    TT_VPIStrekkode.RecType = 3:
    FIND VPIStrekkode EXCLUSIVE-LOCK WHERE
      VPIStrekkode.EkstVPILevNr = TT_VPIStrekkode.EkstVPILevNr AND
      VPISTrekkode.VareNr       = TT_VPIStrekkode.VareNr AND
      VPIStrekkode.Kode         = TT_VPIStrekkode.Kode NO-ERROR.
    IF AVAILABLE VPIStrekkode THEN
    DO:
      DELETE VPIStrekkode NO-ERROR.
      DELETE TT_VPIStrekkode.
    END.
  END. /* SLETTEPOSTER */

  /* Behandler Ny/endre poster */
  NY-ENDRE:
  FOR EACH TT_VPIStrekkode WHERE
    TT_VPIStrekkode.RecType = 1:
    FIND VPIStrekkode EXCLUSIVE-LOCK WHERE
      VPIStrekkode.EkstVPILevNr = TT_VPIStrekkode.EkstVPILevNr AND
      VPISTrekkode.VareNr       = TT_VPIStrekkode.VareNr AND
      VPIStrekkode.Kode         = TT_VPIStrekkode.Kode NO-ERROR.
    /* Legger inn artikkelinformasjonen */
    IF NOT AVAILABLE VPIStrekkode THEN
      CREATE VPIStrekkode.
    BUFFER-COPY TT_VPIStrekkode TO VPIStrekkode.
    RELEASE VPIStrekkode.
    DELETE TT_VPIStrekkode.
  END. /* NY-ENDRE */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-TellOppLinjer) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TellOppLinjer Procedure 
PROCEDURE TellOppLinjer :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR pcTekst AS CHAR NO-UNDO.
  DEF VAR piError AS INT  NO-UNDO.

  ASSIGN
      iTotAntLinjer = 0
      .
  INPUT STREAM InnFil FROM VALUE(VPIFilHode.Katalog + "~\" + VPIFilHode.FilNavn) NO-ECHO.
  REPEAT ON ERROR UNDO, RETRY:
    /* Ved overføring av bilder blir det kalabaise. Derfor No-ERROR. */
    IMPORT STREAM InnFil 
        ^
        NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
        piError = piError + 1.

    ASSIGN
        iTotAntLinjer = iTotAntLinjer + 1
        .
  END.
  INPUT STREAM InnFil CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

