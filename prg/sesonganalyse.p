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
  DEFINE INPUT  PARAMETER cSesong    AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cButiker   AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cXFilter   AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cXParam    AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER cBeststat  AS CHARACTER  NO-UNDO.
  DEFINE INPUT  PARAMETER dDatoFra1  AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER dDatoTil1  AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER dDatoFra2  AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER dDatoTil2  AS DATE       NO-UNDO.
  DEFINE INPUT  PARAMETER lRapport   AS LOGICAL    NO-UNDO.

  DEFINE TEMP-TABLE TT_Artiklar
      FIELD Artikkelnr AS DECI
      FIELD Vg         AS INTE
      FIELD Verdier    AS CHAR
      INDEX vg IS PRIMARY vg
      INDEX Artikkelnr Artikkelnr.

  DEFINE TEMP-TABLE TT_Vg
      FIELD Vg         AS INTE
      FIELD VgBeskr    AS CHAR
      FIELD Verdier    AS CHAR
      INDEX Vg Vg.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-HarBestillt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD HarBestillt Procedure 
FUNCTION HarBestillt RETURNS LOGICAL
  ( INPUT iButnr AS INTEGER,INPUT iBestNr AS INTEGER,INPUT iBestStat AS INTEGER,OUTPUT cTotAntStr AS CHAR)  FORWARD.

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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Bestanalys1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bestanalys1 Procedure 
PROCEDURE Bestanalys1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: 
         
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER TTH AS HANDLE NO-UNDO.
  DEFINE        VARIABLE  cQuery    AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  lFirst    AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  rRowId    AS ROWID      NO-UNDO.
  DEFINE        VARIABLE  dTotSum   AS DECIMAL    NO-UNDO.
  DEFINE        VARIABLE  hBufferField   AS HANDLE     NO-UNDO.
  DEFINE        VARIABLE  pcFeltListe    AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  pcVerdier      AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  iButikloop     AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  lUtvidetFilter AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  lIkkeTreff     AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  iCount         AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  cTotAntStr     AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  cButikkVerdier AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  cVerdier       AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  iLookup        AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  iSasong        AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE cTmpEntry AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE cSumEntry AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE lVisPerBut AS LOGICAL    NO-UNDO.

  EMPTY TEMP-TABLE TT_Artiklar.
  EMPTY TEMP-TABLE TT_Vg.

  IF NUM-ENTRIES(cXFilter,";") = 2 THEN DO:
      ASSIGN pcFeltListe = ENTRY(1,cXFilter,";")
             pcVerdier   = ENTRY(2,cXFilter,";").
      IF pcFeltListe <> pcVerdier THEN  /* */
          ASSIGN lUtvidetFilter = TRUE.
  END.
  /* cVerdier skall flagga om vi har artikeln har best på artikeln. cVedier kopieras in  */
  /* på TT_Artiklerrecorden. Vi initierar sista entryt med '1' = 'totalbutiken' */
  ASSIGN cButikkVerdier = cButiker + ",99999"
         cVerdier       = ";" + FILL(",;",NUM-ENTRIES(cButiker)).

   /* detta skall göras om och loopas run de två säsongbegreppen samt tidsperiode för respektive säsongbegrepp */
  ASSIGN lVisPerBut = IF NUM-ENTRIES(cXParam,CHR(1)) = 2 THEN ENTRY(2,cXParam,CHR(1)) = "J" ELSE FALSE
         cXParam    = ENTRY(1,cXParam,CHR(1)).
  DO iSasong = 1 TO NUM-ENTRIES(cSesong):
      IF ENTRY(iSasong,cSesong) = "" THEN
          NEXT.
      /* Specialhantering av Vis/But                                       */
      /* Parameter cXParam används för att ge möjlighet att visa per butik */
      /* I de statistiktyper som vi skall 'Vis/But' läggs CHR(1)J          */
      FOR EACH ArtBas WHERE ArtBas.Sasong = INT(ENTRY(iSasong,cSesong)) NO-LOCK:
          FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
          IF NOT AVAIL LevBas THEN
              NEXT.
          IF lUtvidetFilter THEN DO:
              FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
              FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
              FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
              FIND VgKat WHERE VgKat.Vg = ArtBas.Vg AND VgKat.VgKat = ArtBas.VgKat NO-LOCK NO-ERROR.
              FIND Kategori OF VgKat NO-LOCK NO-ERROR.
              FIND Sasong OF ArtBas NO-LOCK NO-ERROR.
          END.
          IF lUtvidetFilter THEN DO iCount = 1 TO NUM-ENTRIES(pcFeltListe,CHR(1)):
              ASSIGN lIkkeTreff = FALSE.
              CASE ENTRY(iCount,pcFeltListe,CHR(1)):
                  WHEN "AvdelingNr" THEN DO:
                      IF NOT AVAIL Avdeling OR NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(Avdeling.AvdelingNr)) THEN
                          ASSIGN lIkkeTreff = TRUE.
                  END.
                  WHEN "HuvGr" THEN DO:
                      IF NOT AVAIL HuvGr OR NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(HuvGr.Hg)) THEN
                          ASSIGN lIkkeTreff = TRUE.
                  END.
                  WHEN "VarGr" THEN DO:
                      IF NOT AVAIL VarGr OR NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(VarGr.Vg)) THEN
                          ASSIGN lIkkeTreff = TRUE.
                  END.
                  WHEN "LevNr" THEN DO:
                      IF NOT AVAIL LevBas OR NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(ArtBas.LevNr)) THEN
                          ASSIGN lIkkeTreff = TRUE.
                  END.
                  WHEN "Aktivitet" THEN DO:
                      IF NOT AVAIL VarGr OR NOT CAN-FIND(FIRST VgAkt OF VarGr) THEN DO:
                          ASSIGN lIkkeTreff = TRUE.
                      END.
                      IF lIkkeTreff = FALSE THEN DO:
                          ASSIGN lIkkeTreff = TRUE.
                          FOR EACH VgAkt OF VarGr NO-LOCK:
                              IF CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(VgAkt.AktNr)) THEN DO:
                                  ASSIGN lIkkeTreff = FALSE.
                                  LEAVE.
                              END.
                          END.
                      END.
                  END.
                  WHEN "Kategori" THEN DO:
                      IF NOT AVAIL VarGr OR NOT CAN-FIND(FIRST VgKat OF VarGr) THEN DO:
                          ASSIGN lIkkeTreff = TRUE.
                      END.
                      IF lIkkeTreff = FALSE THEN DO:
                          ASSIGN lIkkeTreff = TRUE.
                          FOR EACH VgKat OF VarGr NO-LOCK:
                              IF CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(VgKat.KatNr)) THEN DO:
                                  ASSIGN lIkkeTreff = FALSE.
                                  LEAVE.
                              END.
                          END.
                      END.
                  END.
              END CASE.
              IF lIkkeTreff THEN
                  LEAVE.
          END.
          IF lIkkeTreff THEN
              NEXT.
          /* Vid lRapport = true skall vi bara skapa records med artiklar - ingen hantering av beställningar */
          IF lRapport = FALSE THEN DO iCount = 1 TO NUM-ENTRIES(cButiker):
              FOR EACH BestHode OF ArtBas NO-LOCK:
                  IF NOT CAN-DO(cBeststat,STRING(BestHode.BestStat)) THEN
                      NEXT.
                  IF dDatoFra1 <> ? AND BestHode.bestillingsdato < dDatoFra1 THEN
                      NEXT.
                  IF dDatoTil1 <> ? AND BestHode.bestillingsdato > dDatoTil1 THEN
                      NEXT.
                  IF NOT HarBestillt(INT(ENTRY(iCount,cButiker)),BestHode.BestNr,BestHode.BestStat,cTotAntStr) THEN NEXT.
                  DO:
                      /* Här har butiken beställt */
                      FIND TT_Artiklar WHERE TT_Artiklar.Artikkelnr = ArtBas.Artikkelnr NO-ERROR.
                      IF NOT AVAIL TT_Artiklar THEN DO:
                          CREATE TT_Artiklar.
                          ASSIGN TT_Artiklar.Artikkelnr = Artbas.Artikkelnr
                                 TT_Artiklar.Vg         = ArtBas.Vg
                                 TT_Artiklar.Verdier    = cVerdier.
/*                           cTmpEntry = ENTRY(NUM-ENTRIES(TT_Artiklar.Verdier),TT_Artiklar.Verdier). */
/*                           ENTRY(iSasong,cTmpEntry,";") = "1".                                      */
/*                           ENTRY(NUM-ENTRIES(TT_Artiklar.Verdier),TT_Artiklar.Verdier) = cTmpEntry. */
                      END.
                      iLookup = LOOKUP(ENTRY(iCount,cButiker),cButikkVerdier).
                      IF iLookup > 0 THEN DO:
                          /* BUT */
                          cTmpEntry = ENTRY(iLookup,TT_Artiklar.Verdier).
                          ENTRY(iSasong,cTmpEntry,";") = "1".
                          ENTRY(iLookup,TT_Artiklar.Verdier) = cTmpEntry.
                          /* TOT */
                          cTmpEntry = ENTRY(NUM-ENTRIES(TT_Artiklar.Verdier),TT_Artiklar.Verdier).
                          ENTRY(iSasong,cTmpEntry,";") = "1".
                          ENTRY(NUM-ENTRIES(TT_Artiklar.Verdier),TT_Artiklar.Verdier) = cTmpEntry.
                      END.
                  END.
              END.
          END.
          ELSE DO:
              FIND TT_Artiklar WHERE TT_Artiklar.Artikkelnr = ArtBas.Artikkelnr NO-ERROR.
              IF NOT AVAIL TT_Artiklar THEN DO:
                  CREATE TT_Artiklar.
                  ASSIGN TT_Artiklar.Artikkelnr = Artbas.Artikkelnr
                         TT_Artiklar.Vg         = ArtBas.Vg.
              END.
          END.
          RELEASE Avdeling.
          RELEASE HuvGr.
          RELEASE VarGr.
          RELEASE VgKat.
          RELEASE Kategori.

      END.
  END.
  IF lRapport = FALSE THEN DO:
      FOR EACH TT_Artiklar.
          FIND TT_Vg WHERE TT_Vg.Vg = TT_Artiklar.Vg NO-ERROR.
          IF NOT AVAIL TT_Vg THEN DO:
              FIND VarGr WHERE vargr.vg = TT_Artiklar.vg NO-LOCK NO-ERROR.
              CREATE TT_Vg.
              ASSIGN TT_Vg.Vg = TT_Artiklar.Vg
                     TT_vg.vgbeskr = IF AVAIL vargr THEN vargr.vgbeskr ELSE ""
                     TT_Vg.Verdier = cVerdier.
          END.
          DO iCount = 1 TO NUM-ENTRIES(TT_Vg.Verdier):

    /*           cTmpEntry = ENTRY(iLookup,TT_Artiklar.Verdier). */
    /*           ENTRY(iSasong,cTmpEntry,";") = "1".             */
    /*           ENTRY(iLookup,TT_Artiklar.Verdier) = cTmpEntry. */

              cTmpEntry = ENTRY(iCount,TT_Vg.Verdier).
              IF ENTRY(1,ENTRY(iCount,TT_Artiklar.Verdier),";") <> "" THEN
                  ENTRY(1,cTmpEntry,";") = STRING(INT(ENTRY(1,cTmpEntry,";")) + INT(ENTRY(1,ENTRY(iCount,TT_Artiklar.Verdier),";"))).
              IF ENTRY(2,ENTRY(iCount,TT_Artiklar.Verdier),";") <> "" THEN
                  ENTRY(2,cTmpEntry,";") = STRING(INT(ENTRY(2,cTmpEntry,";")) + INT(ENTRY(2,ENTRY(iCount,TT_Artiklar.Verdier),";"))).
              ENTRY(iCount,TT_Vg.Verdier) = cTmpEntry.
          END.
      END.
      FOR EACH TT_Vg:
          TT_Vg.verdier = REPLACE(TT_Vg.Verdier,",",CHR(2)).
          TT_Vg.verdier = REPLACE(TT_Vg.Verdier,";",CHR(3)).
      END.
      ASSIGN TTH = BUFFER TT_Vg:HANDLE.
  END.
  ELSE
      ASSIGN TTH = BUFFER TT_Artiklar:HANDLE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Salgsanalys1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Salgsanalys1 Procedure 
PROCEDURE Salgsanalys1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes: 
         
------------------------------------------------------------------------------*/
  DEFINE OUTPUT PARAMETER TTH AS HANDLE NO-UNDO.
  DEFINE        VARIABLE  cQuery    AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  lFirst    AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  rRowId    AS ROWID      NO-UNDO.
  DEFINE        VARIABLE  dTotSum   AS DECIMAL    NO-UNDO.
  DEFINE        VARIABLE  hBufferField   AS HANDLE     NO-UNDO.
  DEFINE        VARIABLE  pcFeltListe    AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  pcVerdier      AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  iButikloop     AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  lUtvidetFilter AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  lIkkeTreff     AS LOGICAL    NO-UNDO.
  DEFINE        VARIABLE  iCount         AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  cTotAntStr     AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  cButikkVerdier AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  cVerdier       AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  iLookup        AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  iSasong        AS INTEGER    NO-UNDO.
  DEFINE        VARIABLE  cTmpEntry      AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  cSumEntry      AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  dAntall        AS DECIMAL    NO-UNDO.
  DEFINE        VARIABLE  cTTid          AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE dDatoFra        AS DATE       NO-UNDO.
  DEFINE        VARIABLE dDatoTil        AS DATE       NO-UNDO.
  DEFINE        VARIABLE ii AS INTEGER    NO-UNDO.
  DEFINE VARIABLE lVisPerBut AS LOGICAL    NO-UNDO.

  EMPTY TEMP-TABLE TT_Artiklar.
  EMPTY TEMP-TABLE TT_Vg.

  cTTiD = "1,3,10".

  IF NUM-ENTRIES(cXFilter,";") = 2 THEN DO:
      ASSIGN pcFeltListe = ENTRY(1,cXFilter,";")
             pcVerdier   = ENTRY(2,cXFilter,";").
      IF pcFeltListe <> pcVerdier THEN  /* */
          ASSIGN lUtvidetFilter = TRUE.
  END.
  /* cVerdier skall flagga om vi har artikeln har best på artikeln. cVedier kopieras in  */
  /* på TT_Artiklerrecorden. Vi initierar sista entryt med '1' = 'totalbutiken' */
  ASSIGN cButikkVerdier = cButiker + ",99999"
         cVerdier       = ";" + FILL(",;",NUM-ENTRIES(cButiker)).

   /* detta skall göras om och loopas run de två säsongbegreppen samt tidsperiode för respektive säsongbegrepp */
  ASSIGN lVisPerBut = IF NUM-ENTRIES(cXParam,CHR(1)) = 2 THEN ENTRY(2,cXParam,CHR(1)) = "J" ELSE FALSE
         cXParam    = ENTRY(1,cXParam,CHR(1)).
/* cQuery = "FOR EACH translogg NO-LOCK WHERE translogg.artikkelnr = " + STRING(Artbas.artikkelnr).                                        */
/* IF dDatoFra <> ? AND dDatoTil <> ? THEN                                                                                                 */
/*     cQuery = cQuery + " AND translogg.dato >= date('" + string(dDatoFra) + "') AND translogg.dato <= date('" + string(dDatoTil) + "')". */
/* ELSE IF dDatoFra <> ? THEN                                                                                                              */
/*     cQuery = cQuery + " AND translogg.dato >= date('" + string(dDatoFra) + "')".                                                        */
/* ELSE IF dDatoTil <> ? THEN                                                                                                              */
/*     cQuery = cQuery + " AND translogg.dato <= date('" + string(dDatoTil) + "')".                                                        */

  DO iSasong = 1 TO NUM-ENTRIES(cSesong):
      IF ENTRY(iSasong,cSesong) = "" THEN
          NEXT.
      ASSIGN dDatoFra = IF iSasong = 1 THEN dDatoFra1 ELSE dDatoFra2
             dDatoTil = IF iSasong = 2 THEN dDatoTil1 ELSE dDatoTil2.
      /* Specialhantering av Vis/But                                       */
      /* Parameter cXParam används för att ge möjlighet att visa per butik */
      /* I de statistiktyper som vi skall 'Vis/But' läggs CHR(1)J          */
      FOR EACH ArtBas WHERE ArtBas.Sasong = INT(ENTRY(iSasong,cSesong)) NO-LOCK:
          FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
          IF NOT AVAIL LevBas THEN
              NEXT.
          IF lUtvidetFilter THEN DO:
              FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
              FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
              FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
              FIND VgKat WHERE VgKat.Vg = ArtBas.Vg AND VgKat.VgKat = ArtBas.VgKat NO-LOCK NO-ERROR.
              FIND Kategori OF VgKat NO-LOCK NO-ERROR.
              FIND Sasong OF ArtBas NO-LOCK NO-ERROR.
          END.
          IF lUtvidetFilter THEN DO iCount = 1 TO NUM-ENTRIES(pcFeltListe,CHR(1)):
              ASSIGN lIkkeTreff = FALSE.
              CASE ENTRY(iCount,pcFeltListe,CHR(1)):
                  WHEN "AvdelingNr" THEN DO:
                      IF NOT AVAIL Avdeling OR NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(Avdeling.AvdelingNr)) THEN
                          ASSIGN lIkkeTreff = TRUE.
                  END.
                  WHEN "HuvGr" THEN DO:
                      IF NOT AVAIL HuvGr OR NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(HuvGr.Hg)) THEN
                          ASSIGN lIkkeTreff = TRUE.
                  END.
                  WHEN "VarGr" THEN DO:
                      IF NOT AVAIL VarGr OR NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(VarGr.Vg)) THEN
                          ASSIGN lIkkeTreff = TRUE.
                  END.
                  WHEN "LevNr" THEN DO:
                      IF NOT AVAIL LevBas OR NOT CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(ArtBas.LevNr)) THEN
                          ASSIGN lIkkeTreff = TRUE.
                  END.
                  WHEN "Aktivitet" THEN DO:
                      IF NOT AVAIL VarGr OR NOT CAN-FIND(FIRST VgAkt OF VarGr) THEN DO:
                          ASSIGN lIkkeTreff = TRUE.
                      END.
                      IF lIkkeTreff = FALSE THEN DO:
                          ASSIGN lIkkeTreff = TRUE.
                          FOR EACH VgAkt OF VarGr NO-LOCK:
                              IF CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(VgAkt.AktNr)) THEN DO:
                                  ASSIGN lIkkeTreff = FALSE.
                                  LEAVE.
                              END.
                          END.
                      END.
                  END.
                  WHEN "Kategori" THEN DO:
                      IF NOT AVAIL VarGr OR NOT CAN-FIND(FIRST VgKat OF VarGr) THEN DO:
                          ASSIGN lIkkeTreff = TRUE.
                      END.
                      IF lIkkeTreff = FALSE THEN DO:
                          ASSIGN lIkkeTreff = TRUE.
                          FOR EACH VgKat OF VarGr NO-LOCK:
                              IF CAN-DO(ENTRY(iCount,pcVerdier,CHR(1)),STRING(VgKat.KatNr)) THEN DO:
                                  ASSIGN lIkkeTreff = FALSE.
                                  LEAVE.
                              END.
                          END.
                      END.
                  END.
              END CASE.
              IF lIkkeTreff THEN
                  LEAVE.
          END.
          IF lIkkeTreff THEN
              NEXT.

/*           !!! */
          IF dDatoFra = ? THEN DO:
              FIND FIRST Translogg  WHERE Translogg.Artikkelnr = ArtBas.Artikkelnr USE-INDEX OppslagDatoTid NO-LOCK NO-ERROR.
              IF NOT AVAIL Translogg THEN
                  NEXT.
              ASSIGN dDatoFra = Translogg.Dato.
          END.
          dDatoTil = IF dDatoTil = ? THEN TODAY ELSE dDatoTil.
/*           !!! */
          FIND vargr OF artbas NO-LOCK NO-ERROR.
          FIND TT_Vg WHERE TT_Vg.Vg = ArtBas.Vg NO-ERROR.
          DO:
              FOR EACH translogg WHERE translogg.artikkelnr = ArtBas.artikkelnr AND 
                                       translogg.dato       >= dDatoFra AND translogg.dato <= dDatoTil NO-LOCK:
                  IF CAN-DO(cButiker,STRING(Translogg.Butik)) AND CAN-DO(cTTid,STRING(Translogg.TTid)) THEN DO:
                      /* Här har butiken beställt */
                      IF NOT AVAIL TT_Vg THEN DO:
                          CREATE TT_Vg.
                          ASSIGN TT_Vg.Vg         = ArtBas.Vg
                                 TT_vg.vgbeskr    = IF AVAIL vargr THEN vargr.vgbeskr ELSE "".
                                 TT_Vg.Verdier    = cVerdier.
    /*                           cTmpEntry = ENTRY(NUM-ENTRIES(TT_Artiklar.Verdier),TT_Artiklar.Verdier). */
    /*                           ENTRY(iSasong,cTmpEntry,";") = "1".                                      */
    /*                           ENTRY(NUM-ENTRIES(TT_Artiklar.Verdier),TT_Artiklar.Verdier) = cTmpEntry. */
                      END.
                      iLookup = LOOKUP(STRING(Translogg.butik),cButikkVerdier).
                      IF iLookup > 0 THEN DO:
                          dAntall = Translogg.antall.
                          /* BUT */
                          cTmpEntry = ENTRY(iLookup,TT_Vg.Verdier).
                          ENTRY(iSasong,cTmpEntry,";") = STRING(DECI(ENTRY(iSasong,cTmpEntry,";")) + dAntall).
                          ENTRY(iLookup,TT_Vg.Verdier) = cTmpEntry.
                          /* TOT */
                          cTmpEntry = ENTRY(NUM-ENTRIES(TT_Vg.Verdier),TT_Vg.Verdier).
                          ENTRY(iSasong,cTmpEntry,";") = STRING(DECI(ENTRY(iSasong,cTmpEntry,";")) + dAntall).
                          ENTRY(NUM-ENTRIES(TT_Vg.Verdier),TT_Vg.Verdier) = cTmpEntry.
                      END.
                  END.
              END.
          END.
          RELEASE Avdeling.
          RELEASE HuvGr.
          RELEASE VarGr.
          RELEASE VgKat.
          RELEASE Kategori.

      END.
  END.
  OUTPUT TO "CLIPBOARD".
  
/*   FOR EACH TT_Artiklar.                                                                                                           */
/*       EXPORT TT_Artiklar.                                                                                                         */
/*       FIND TT_Vg WHERE TT_Vg.Vg = TT_Artiklar.Vg NO-ERROR.                                                                        */
/*       IF NOT AVAIL TT_Vg THEN DO:                                                                                                 */
/*           CREATE TT_Vg.                                                                                                           */
/*           ASSIGN TT_Vg.Vg = TT_Artiklar.Vg                                                                                        */
/*                  TT_Vg.Verdier = cVerdier.                                                                                        */
/*       END.                                                                                                                        */
/*       DO iCount = 1 TO NUM-ENTRIES(TT_Vg.Verdier):                                                                                */
/*                                                                                                                                   */
/* /*           cTmpEntry = ENTRY(iLookup,TT_Artiklar.Verdier). */                                                                   */
/* /*           ENTRY(iSasong,cTmpEntry,";") = "1".             */                                                                   */
/* /*           ENTRY(iLookup,TT_Artiklar.Verdier) = cTmpEntry. */                                                                   */
/*                                                                                                                                   */
/*           cTmpEntry = ENTRY(iCount,TT_Vg.Verdier).                                                                                */
/*           IF ENTRY(1,ENTRY(iCount,TT_Artiklar.Verdier),";") <> "" THEN                                                            */
/*               ENTRY(1,cTmpEntry,";") = STRING(INT(ENTRY(1,cTmpEntry,";")) + INT(ENTRY(1,ENTRY(iCount,TT_Artiklar.Verdier),";"))). */
/*           IF ENTRY(2,ENTRY(iCount,TT_Artiklar.Verdier),";") <> "" THEN                                                            */
/*               ENTRY(2,cTmpEntry,";") = STRING(INT(ENTRY(2,cTmpEntry,";")) + INT(ENTRY(2,ENTRY(iCount,TT_Artiklar.Verdier),";"))). */
/*           ENTRY(iCount,TT_Vg.Verdier) = cTmpEntry.                                                                                */
/*       END.                                                                                                                        */
/*   END.                                                                                                                            */
  FOR EACH TT_Vg:
      EXPORT TT_Vg.
      TT_Vg.verdier = REPLACE(TT_Vg.Verdier,",",CHR(2)).
      TT_Vg.verdier = REPLACE(TT_Vg.Verdier,";",CHR(3)).
  END.
  OUTPUT CLOSE.
  ASSIGN TTH = BUFFER TT_Vg:HANDLE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-HarBestillt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION HarBestillt Procedure 
FUNCTION HarBestillt RETURNS LOGICAL
  ( INPUT iButnr AS INTEGER,INPUT iBestNr AS INTEGER,INPUT iBestStat AS INTEGER,OUTPUT cTotAntStr AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes: OBS: rekkefölge i utstring = rekkefölge define 
------------------------------------------------------------------------------*/
  DEFINE VARIABLE  dTotAntPar     AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE  dTotDbKr       AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE  dTotInnkjVerdi AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE  dTotInnLev     AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE  dTotMakulert   AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE  dTotOverLev    AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE  dTotSalgsVerdi AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE  iKoeff         AS INTEGER    NO-UNDO.
  IF NOT CAN-FIND(FIRST Beststr WHERE Beststr.BestNr   = iBestNr AND
                                      Beststr.Butik    = iButNr  AND
                                      Beststr.BestStat = iBestStat AND
                                      BestStr.Bestilt  > 0) THEN
      ASSIGN cTotAntStr = "".
  ELSE DO:
        ASSIGN iKoeff = IF iBestStat = 7 THEN -1 ELSE 1.
        FIND BestPris WHERE BestPris.Bestnr = iBestNr AND BestPris.BestStat = iBestStat NO-LOCK NO-ERROR.
        FOR EACH Beststr WHERE Beststr.BestNr   = iBestNr   AND Beststr.Butik   = iButNr  AND
                               Beststr.BestStat = iBestStat AND BestStr.Bestilt > 0 NO-LOCK:
            ASSIGN dTotAntPar = dTotAntPar + BestStr.Bestilt.
        END.
        FOR EACH BestLevert WHERE BestLevert.BestNr = iBestNr AND BestLevert.Butik = iButNr NO-LOCK:
            ASSIGN dTotInnLev   = dTotInnLev + BestLevert.Levert
                   dTotMakulert = dTotMakulert + IF BestLevert.Levert = 0 THEN BestLevert.Rest ELSE 0.
        END.
        ASSIGN dTotInnkjVerdi = dTotAntPar * BestPris.VareKost
               dTotOverLev    = IF iBestStat <> 7 THEN dTotInnLev + dTotMakulert - dTotAntPar ELSE 0
               dTotOverLev    = IF dTotOverLev < 0 THEN 0 ELSE dTotOverLev
               dTotSalgsVerdi = dTotAntPar * BestPris.Pris
               dTotDbKr       = dTotSalgsVerdi - dTotAntPar * BestPris.MvaKr - dTotInnkjVerdi.
        ASSIGN cTotAntStr = STRING(iKoeff * dTotAntPar)     + CHR(1) +
                            STRING(iKoeff * dTotDbKr)       + CHR(1) +
                            STRING(iKoeff * dTotInnkjVerdi) + CHR(1) +
                            STRING(iKoeff * dTotInnLev)     + CHR(1) +
                            STRING(iKoeff * dTotMakulert)   + CHR(1) +
                            STRING(iKoeff * dTotOverLev)    + CHR(1) +
                            STRING(iKoeff * dTotSalgsVerdi).
  END.
  RETURN cTotAntStr <> "".
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

