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
DEFINE INPUT PARAMETER cButLst AS CHARACTER NO-UNDO.

DEFINE VARIABLE wStTypeListe AS CHAR    NO-UNDO.
DEFINE VARIABLE iAarPerLinNr AS INTEGER NO-UNDO.
DEFINE VARIABLE lTotAnt      AS DECIMAL NO-UNDO.
DEFINE VARIABLE lVVareKost   AS DECIMAL NO-UNDO.

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

DEF VAR wLoop1      AS INT  NO-UNDO.
DEF VAR wDataObjekt AS CHAR NO-UNDO.
DEF VAR wWork1      AS DEC  NO-UNDO.
DEF VAR wWork2      AS DEC  NO-UNDO.
DEF VAR wWork3      AS DEC  NO-UNDO.

IF TRIM(cButLst) = '' THEN 
  RETURN 'Ingen butikker i butikkliste.'.

/* Liste over de StTypeId som det skal bygges StLAger for. */
ASSIGN
    /*wStTypeListe = "BUTSTAT,HOVEDGR,LEVERAN,LEVERAN-VG,VAREGR,AVDELING"*/
    wStTypeListe = "BUTSTAT,HOVEDGR,LEVERAN,LEVERAN-VG,VAREGR,AVDELING"
    iAarPerLinNr = 0 /* Statistiks lager skal ha null. Senere vil historisk lager ha <> 0 */
    /*
    iAarPerLinNr = int(
                       STRING(YEAR(TODAY),"9999") + STRING(TODAY - DATE(12,31,YEAR(TODAY) - 1),"999")
                       )
    */                                          
    .

LES_BUTIKKER:
FOR EACH Butiker NO-LOCK WHERE
  CAN-DO(cButLst,STRING(Butiker.Butik)):

  IF Butiker.HarButikkSystem = FALSE THEN NEXT LES_BUTIKKER.  
  IF Butiker.Apningsdato     = ?     THEN NEXT LES_BUTIKKER. 
  IF Butiker.Apningsdato     > TODAY THEN NEXT LES_BUTIKKER. 
  IF (Butiker.NedlagtDato <> ? AND
      Butiker.NedlagtDato    <= TODAY) THEN NEXT LES_BUTIKKER. 

  PUBLISH 'infoDisp' ("SLETTER StLager for butikk: " + string(Butiker.Butik) + " " + Butiker.ButNamn + ".").

  /* Renser bort StLager for butikken.                      */
  /* NB: iAarPerLinNr skal senere avgrenses for bare 0 her. */
  RENS:
  DO wLoop1 = 1 TO NUM-ENTRIES(wStTypeListe):
    FOR EACH StLager EXCLUSIVE-LOCK WHERE
      StLager.StTypeId = ENTRY(wLoop1,wStTypeListe) AND
      StLager.Butik    = Butiker.Butik:
      
      DELETE StLager.      
    END.
  END. /* RENS */
  
  BYGG:
  FOR EACH Lager NO-LOCK WHERE 
    Lager.Butik = Butiker.Butik:
    FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = Lager.ArtikkelNr NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN NEXT BYGG.
    IF (ArtBas.OPris = TRUE OR ArtBas.Lager = FALSE) THEN NEXT BYGG.
    IF Lager.LagAnt <= 0 THEN NEXT BYGG.
  
    PUBLISH 'infoDisp' ("BYGGER StLager for Artikkel: " + string(Artbas.ArtikkelNr) + " " + ArtBas.Beskr + " Vg: " + STRING(ArtBas.Vg) + ".").
  
    STLOOP:
    DO wLoop1 = 1 TO NUM-ENTRIES(wStTypeListe):
        /* Setter dataobjekt */
        CASE ENTRY(wLoop1,wStTypeListe):
          WHEN "AVDELING"  THEN
            DO:
              FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
              IF NOT AVAIL VarGr THEN
                  NEXT STLOOP.
              FIND HuvGr OF VarGr NO-LOCK NO-ERROR.
              IF NOT AVAIL HuvGr THEN
                  NEXT STLOOP.
              IF CAN-FIND(Avdeling OF HuvGr) THEN
                  wDataObjekt = STRING(HuvGr.AvdelingNr,"9999").
              ELSE NEXT STLOOP.
            END.
          WHEN "HOVEDGR"  THEN
            DO:
              FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
              IF NOT AVAIL VarGr THEN
                  NEXT STLOOP.
              IF CAN-FIND(HuvGr OF VarGr) THEN
                wDataObjekt = STRING(VarGr.Hg,"9999").
              ELSE NEXT STLOOP.
            END.
          WHEN "VAREGR"   THEN
            wDataObjekt = STRING(ArtBas.Vg,"999999").
          WHEN "LEVERAN"  THEN
            wDataObjekt = STRING(ArtBas.LevNr,"999999").
          WHEN "BUTSTAT" THEN
            wDataObjekt = STRING(Lager.Butik,"999999").
          WHEN "LEVERAN-VG"  THEN
            wDataObjekt = STRING(ArtBas.LevNr,"999999") + CHR(1) + 
                            string(ArtBas.Vg,"999999").
        END CASE.

        FIND StLager EXCLUSIVE-LOCK WHERE
            StLager.AarPerLinNr = iAarPerLinNr AND 
            StLager.StTypeId    = ENTRY(wLoop1,wStTypeListe) AND
            StLager.DataObjekt  = wDataObjekt AND
            StLager.Butik       = Lager.Butik NO-ERROR.
        IF NOT AVAILABLE StLager THEN
        DO:
            CREATE StLager.
            ASSIGN
                StLager.AarPerLinNr = iAarPerLinNr  
                StLager.StTypeId    = ENTRY(wLoop1,wStTypeListe) 
                StLager.DataObjekt  = wDataObjekt 
                StLager.Butik       = Lager.Butik.
        END.
        
        /* Akkumulerer lagerantall. Teller bare pos. lagerantall */
        ARTLAGLOOP:
        FOR EACH ArtLag NO-LOCK WHERE
          ArtLag.ArtikkelNr = Lager.ArtikkelNr AND 
          ArtLag.Butik      = Lager.Butik: 
          IF ArtLag.LagAnt <= 0 THEN NEXT ARTLAGLOOP.         
          ASSIGN
            lVVareKost             = ABS(Lager.vVareKost)
            lVVareKost             = (IF lVVareKost = ? THEN 0 ELSE lVVareKost)
            StLager.LagAnt         = StLager.LagAnt         + ArtLag.LagAnt      
            StLager.VVareKost      = StLager.VVareKost      + (ArtLag.LagAnt * lVVareKost)
            .
        END.

        /* Akkumulerer antall og verdi */
        ASSIGN
          StLager.vSnittKostPris = StLager.vSnittKostPris + (Lager.AntSolgt * Lager.vVareKost)
          StLager.AntSolgt       = StLager.AntSolgt       + Lager.AntSolgt
          StLager.IntAnt         = StLager.IntAnt         + Lager.IntAnt
          StLager.ReklAnt        = StLager.ReklAnt        + Lager.ReklAnt
          StLager.ReklLAnt       = StLager.ReklLAnt       + Lager.ReklLAnt
          StLager.RetLAnt        = StLager.RetLAnt        + Lager.RetLAnt
          StLager.OvAnt          = StLager.OvAnt          + Lager.OvAnt
          StLager.JustAnt        = StLager.JustAnt        + Lager.JustAnt
          StLager.SvinnAnt       = StLager.SvinnAnt       + Lager.SvinnAnt
          StLager.NedAnt         = StLager.NedAnt         + Lager.NedAnt
          StLager.KjopAnt        = StLager.KjopAnt        + Lager.KjopAnt
          StLager.BrekkAnt       = StLager.BrekkAnt       + Lager.BrekkAnt
          StLager.GjenkjopAnt    = StLager.GjenkjopAnt    + Lager.GjenkjopAnt
          StLager.AntRab         = StLager.AntRab         + Lager.AntRab
          .
        
        /* Akkumulerer verdi */
        ASSIGN
          StLager.BrekkVerdi     = StLager.BrekkVerdi     + Lager.BrekkVerdi
          StLager.JustVerdi      = StLager.JustVerdi      + Lager.JustVerdi
          StLager.SvinnVerdi     = StLager.SvinnVerdi     + Lager.SvinnVerdi
          StLager.NedVerdi       = StLager.NedVerdi       + Lager.NedVerdi
          StLager.KjopVerdi      = StLager.KjopVerdi      + Lager.KjopVerdi
          StLager.IntVerdi       = StLager.IntVerdi       + Lager.IntVerdi
          StLager.GjenkjopVerdi  = StLager.GjenkjopVerdi  + Lager.GjenkjopVerdi
          StLager.ReklVerdi      = StLager.ReklVerdi      + Lager.ReklVerdi
          StLager.ReklLVerdi     = StLager.ReklLVerdi     + Lager.ReklLVerdi
          StLager.OvVerdi        = StLager.OvVerdi        + Lager.OvVerdi
          StLager.VerdiRabatt    = StLager.VerdiRabatt    + Lager.VerdiRabatt
          StLager.SVK            = StLager.SVK            + Lager.SVK
          StLager.VerdiSolgt     = StLager.VerdiSolgt     + Lager.VerdiSolgt
          .
    END. /* STLOOP */
  
  END. /* BYGG */
END. /* LES_BUTIKKER */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


