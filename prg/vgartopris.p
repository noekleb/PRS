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

DEFINE INPUT  PARAMETER ipArtikkelNr LIKE ArtBas.ArtikkelNr NO-UNDO.
DEFINE INPUT  PARAMETER ipVg         LIKE VarGr.Vg          NO-UNDO.

DEFINE VARIABLE iStrTypeId AS INTEGER    NO-UNDO.
DEFINE VARIABLE iCL        AS INTEGER    NO-UNDO.
DEFINE VARIABLE iProfilNr  AS INTEGER    NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-SetLopeNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD SetLopeNr Procedure 
FUNCTION SetLopeNr RETURNS INTEGER
  ( /* parameter-definitions */ )  FORWARD.

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
{syspara.i 50 15 1 iStrTypeID INT}
IF iStrTypeId = 0 THEN
    iStrTypeId = 2.
{syspara.i 5 1 1 iCL INT}
FIND Butiker NO-LOCK WHERE Butiker.Butik = iCL.
ASSIGN iProfilNr = Butiker.ProfilNr.

/* Oppretter pluLeverandør */
IF NOT CAN-FIND(LevBas WHERE LevBas.LevNr = 999999) THEN
DO TRANSACTION:
  CREATE LevBas.
  ASSIGN
    LevBas.LevNr = 999999
    LevBas.LevNamn = "PLU leverandør"
    .
  FIND CURRENT LevBas NO-LOCK.
END. /* TRANSACTION */

IF ipArtikkelNr <> ? THEN DO: /* Vid ny Opris ikke vg artikkel, skapa kalkyle */
    FIND ArtBas WHERE ArtBas.ArtikkelNr = ipArtikkelNr NO-LOCK NO-ERROR.
    IF ArtBas.Opris = FALSE THEN
        RETURN.
    FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
    IF NOT AVAIL VarGr THEN
        RETURN.
    FIND Moms OF VarGr NO-LOCK NO-ERROR.
    IF NOT AVAIL Moms THEN
        RETURN.
    IF CAN-FIND(FIRST ArtPris OF ArtBas) THEN
        RETURN.
    RUN Kalkyle.
END.
ELSE IF ipVg <> ? THEN DO: /* upplägg/vedlikehold av en vgart*/
    FIND VarGr WHERE VarGr.Vg = ipVg NO-LOCK NO-ERROR.
    IF NOT AVAIL VarGr THEN
        RETURN.
    IF CAN-FIND(StrekKode WHERE StrekKode.Kode = STRING(VarGr.Vg) AND 
                                StrekKode.ArtikkelNr <> VarGr.Vg) THEN
        RETURN.
    FIND Moms OF VarGr NO-LOCK NO-ERROR.
    IF NOT AVAIL Moms THEN
        RETURN.
    RUN VgArtVedlikehold.
END.
ELSE DO:
    FOR EACH VarGr NO-LOCK:
        FIND Moms OF VarGr NO-LOCK NO-ERROR.
        IF CAN-FIND(StrekKode WHERE StrekKode.Kode = STRING(VarGr.Vg) AND 
                                    StrekKode.ArtikkelNr <> VarGr.Vg) THEN
            NEXT.
        IF NOT AVAIL Moms THEN
            NEXT.
        RUN VgArtVedlikehold.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Kalkyle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Kalkyle Procedure 
PROCEDURE Kalkyle :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lNy AS LOGICAL    NO-UNDO.

DO TRANSACTION:
    FIND ArtPris WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                       ArtPris.ProfilNr   = iProfilNr NO-ERROR.
    
    IF NOT AVAIL ArtPris THEN DO:
        CREATE ArtPris.
        ASSIGN  /* nyckelfält */
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr 
            ArtPris.ProfilNr   = iProfilNr
            ArtPris.AktivFraDato    = TODAY
            lNy = TRUE.
    END.
    IF lNy OR ArtBas.Artikkelnr = ArtBas.Vg THEN DO:
        ASSIGN
            ArtPris.DB%[1]          = 100 - IF VarGr.Kost_Proc = 0 THEN 65 ELSE VarGr.Kost_Proc
            ArtPris.DBKr[1]         = ArtPris.DB%[1]
            ArtPris.EuroManuel      = TRUE
            ArtPris.LevNr           = 999999
            ArtPris.Mva%[1]         = Moms.MomsProc
            ArtPris.MvaKr[1]        = Moms.MomsProc
            ArtPris.Pris[1]         = 100 + Moms.MomsProc
            ArtPris.ValPris[1]      = 100 - ArtPris.DB%[1]
            ArtPris.InnkjopsPris[1] = ArtPris.ValPris[1]
            ArtPris.VareKost[1]     = ArtPris.ValPris[1].
    END.
    ELSE DO: /* oppdatering av kalkyle for 'vanlig' Opris-artikkel */
        IF ArtPris.Pris[1] <> 100 + Moms.MomsProc OR ArtPris.DBKr[1] + ArtPris.ValPris[1] <> 100 THEN
            ASSIGN
                ArtPris.DB%[1]          = 100 - IF VarGr.Kost_Proc = 0 THEN 65 ELSE VarGr.Kost_Proc
                ArtPris.DBKr[1]         = ArtPris.DB%[1]
                ArtPris.EuroManuel      = TRUE
                ArtPris.LevNr           = 999999
                ArtPris.Mva%[1]         = Moms.MomsProc
                ArtPris.MvaKr[1]        = Moms.MomsProc
                ArtPris.Pris[1]         = 100 + Moms.MomsProc
                ArtPris.ValPris[1]      = 100 - ArtPris.DB%[1]
                ArtPris.InnkjopsPris[1] = ArtPris.ValPris[1]
                ArtPris.VareKost[1]     = ArtPris.ValPris[1].
    END.
    RELEASE ArtPris.
    
END. /* TRANSACTION */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VgArtVedlikehold) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VgArtVedlikehold Procedure 
PROCEDURE VgArtVedlikehold :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO TRANSACTION:
    FIND ArtBas WHERE ArtBas.ArtikkelNr = VarGr.Vg NO-ERROR.
    /* Oppstandelsen */
    IF NOT AVAIL ArtBas THEN DO:
        CREATE ArtBas.
        ASSIGN ArtBas.ArtikkelNr   = VarGr.Vg
               ArtBas.Vg           = VarGr.Vg
               ArtBas.Hg           = VarGr.Hg
               ArtBas.VgKat        = 1
               ArtBas.LopNr        = ?
               ArtBas.StrTypeId    = iStrTypeId
               ArtBas.SaSong       = 1
               ArtBas.ArtSlag      = 4.
        /* Setter løpenummer */
        ASSIGN ArtBas.LopNr = SetLopeNr().
    END.
    ASSIGN ArtBas.Beskr        = VarGr.VgBeskr
           ArtBas.BongTekst    = TRIM(SUBSTRING(VarGr.VgBeskr,1,30))
           ArtBas.LevKod       = ""   
           ArtBas.VmId         = 0
           ArtBas.LevNr        = 999999
           ArtBas.Notat        = "PLU-artikkel varegruppe " + STRING(VarGr.Vg)
           ArtBas.Farg         = 0
           ArtBas.ValKod       = ""
           ArtBas.Lager        = FALSE
           ArtBas.Storrelser   = TRUE
           ArtBas.Aktivert     = TRUE
           ArtBas.OPris        = TRUE
           .

    /* Oppretter lagerposter */
    FOR EACH Butiker NO-LOCK WHERE
        Butiker.Butik > 0:
        IF NOT CAN-FIND(Lager WHERE Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
                                    Lager.Butik      = Butiker.Butik) THEN DO:
            CREATE Lager.
            ASSIGN Lager.ArtikkelNr = ArtBas.ArtikkelNr
                   Lager.Butik      = Butiker.Butik.
        END.
    END.

    /* Oppretter kalkyle for sentrallageret. */
    RUN Kalkyle.
    IF NOT CAN-FIND(StrekKode WHERE StrekKode.Kode = STRING(VarGr.Vg) AND 
                                StrekKode.ArtikkelNr = VarGr.Vg) THEN DO:
        CREATE StrekKode.
        ASSIGN StrekKode.ArtikkelNr = VarGr.Vg
               StrekKode.Kode       = STRING(VarGr.Vg)
               StrekKode.HovedNr    = TRUE
               StrekKode.VareId     = StrekKode.ArtikkelNr.
    END.
END. /* TRANSACTION */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-SetLopeNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetLopeNr Procedure 
FUNCTION SetLopeNr RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEF VAR wLoop AS INT NO-UNDO.
      
  DEF BUFFER bufArtBas FOR ArtBas.

  FINN-NESTE:  
  REPEAT wLoop = 1 TO 10000:
  
    IF wLoop = 0 THEN
      NEXT FINN-NESTE.
      
    IF CAN-FIND(bufArtBas NO-LOCK WHERE
      bufArtBas.Vg    = ArtBas.Vg AND
      bufArtBas.LopNr = wLoop) THEN
      DO:
        NEXT FINN-NESTE.
      END.
    ELSE
      LEAVE FINN-NESTE.          
  END. /* FINN-NESTE */
  
  IF wLoop > 9999 THEN
      RETURN ?.
  ELSE
      RETURN wLoop.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

