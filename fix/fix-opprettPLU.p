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

DEF VAR cFilNavn    AS CHAR NO-UNDO.
DEF VAR lArtikkelNr AS dec FORMAT ">>>>>>>>>>>>9" NO-UNDO.
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

&IF DEFINED(EXCLUDE-getArtikkelNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getArtikkelNr Procedure 
FUNCTION getArtikkelNr RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

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


ASSIGN
    cFilNavn = "PLU-ArtikklerPressbyran.sdv"
    .

DEF STREAM Inn.


{syspara.i 50 15 1 iStrTypeID INT}
IF iStrTypeId = 0 THEN
    iStrTypeId = 2.
{syspara.i 5 1 1 iCL INT}
FIND Butiker NO-LOCK WHERE Butiker.Butik = iCL.
ASSIGN iProfilNr = Butiker.ProfilNr.

INPUT STREAM Inn FROM VALUE(cFilNavn) NO-ECHO.

REPEAT:
    IMPORT STREAM Inn DELIMITER ";" ^ ^ ^ ^ ^ ^ ^ lArtikkelNr. 

    FIND ArtBas NO-LOCK WHERE
        ArtBas.ArtikkelNr = lArtikkelNr NO-ERROR.
    FIND Strekkode NO-LOCK WHERE
        Strekkode.Kode = string(lArtikkelNr) NO-ERROR.

    PAUSE 0.
    DISPLAY 
        lArtikkelNr
        "Artikkel" WHEN AVAILABLE ArtBas
        "Strekkode" WHEN AVAILABLE Strekkode
        .
    IF AVAILABLE ArtBas OR AVAILABLE Strekkode THEN
        MESSAGE "Match"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

INPUT STREAM Inn CLOSE.

/* OPPRETTELSE AV ARTIKKLER */
PROCEDURE SkapeArtBas:

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-SkapaArtBas) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaArtBas Procedure 
PROCEDURE SkapaArtBas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iVg AS INTEGER    NO-UNDO.
    DEFINE BUFFER bVarGr FOR VarGr.
    FIND bVarGr WHERE bVarGr.Vg = iVg NO-LOCK.
    FIND Moms OF bVarGr NO-LOCK NO-ERROR.
    IF NOT AVAIL Moms THEN DO:
        MESSAGE "Momskode for varegruppe " bVarGr.Vg " feilaktig."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
    END.
    FIND ArtBas WHERE ArtBas.ArtikkelNr = bVarGr.Vg NO-LOCK NO-ERROR.
    IF AVAIL ArtBas THEN
        RETURN.
    /* Oppstandelsen */
    DO:
        CREATE ArtBas.
        ASSIGN ArtBas.ArtikkelNr   = bVarGr.Vg
               ArtBas.Vg           = bVarGr.Vg
               ArtBas.Hg           = bVarGr.Hg
               ArtBas.VgKat        = 1
               ArtBas.LopNr        = ?
               ArtBas.StrTypeId    = iStrTypeId
               ArtBas.SaSong       = 1.
        /* Setter løpenummer */
        ASSIGN ArtBas.LopNr = SetLopeNr().
    END.
    ASSIGN ArtBas.Beskr        = bVarGr.VgBeskr
           ArtBas.BongTekst    = TRIM(SUBSTRING(bVarGr.VgBeskr,1,30))
           ArtBas.LevKod       = ""   
           ArtBas.VmId         = 0
           ArtBas.LevNr        = 999999
           ArtBas.Notat        = "PLU-artikkel varegruppe " + STRING(bVarGr.Vg)
           ArtBas.Farg         = 0
           ArtBas.ValKod       = ""
           ArtBas.Lager        = FALSE
           ArtBas.Storrelser   = TRUE
           ArtBas.Aktivert     = TRUE
           ArtBas.OPris        = TRUE
           .

    /* Oppretter pluLeverandør */
    IF NOT CAN-FIND(LevBas WHERE LevBas.LevNr = 999999) THEN
    DO:
      CREATE LevBas.
      ASSIGN
        LevBas.LevNr = 999999
        LevBas.LevNamn = "PLU leverandør"
        .
    END.

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
    IF NOT CAN-FIND(ArtPris WHERE ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                                  ArtPris.ProfilNr   = iProfilNr) THEN DO:
        CREATE ArtPris.
        ASSIGN  /* nyckelfält */
            ArtPris.ArtikkelNr = ArtBas.ArtikkelNr 
            ArtPris.ProfilNr   = iProfilNr.
        ASSIGN
            ArtPris.AktivFraDato = TODAY
            ArtPris.DB%[1]       = 100 - bVarGr.Kost_Proc
            ArtPris.DBKr[1]      = 100 - bVarGr.Kost_Proc
            ArtPris.EuroManuel   = TRUE
            ArtPris.LevNr        = 999999
            ArtPris.Mva%[1]      = Moms.MomsProc
            ArtPris.MvaKr[1]     = Moms.MomsProc
            ArtPris.Pris[1]      = 100 + Moms.MomsProc
            ArtPris.ValPris[1]   = bVarGr.Kost_Proc
            ArtPris.VareKost[1]  = bVarGr.Kost_Proc.
    END.
    /* Legger opp interne EAN koder på de størrelser som ikke har dette. */
    CREATE StrekKode.
    ASSIGN StrekKode.ArtikkelNr = VarGr.Vg
           StrekKode.Kode       = STRING(VarGr.Vg)
           StrekKode.HovedNr    = TRUE
           StrekKode.VareId     = StrekKode.ArtikkelNr.
/*     RUN genStrekKode.p (ArtBas.ArtikkelNr,1). */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getArtikkelNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getArtikkelNr Procedure 
FUNCTION getArtikkelNr RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  FIND ArtBas WHERE ArtBas.ArtikkelNr = VarGr.Vg NO-LOCK NO-ERROR.
  RETURN IF AVAIL ArtBas THEN STRING(ArtBas.ArtikkelNr) ELSE "".   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SetLopeNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION SetLopeNr Procedure 
FUNCTION SetLopeNr RETURNS INTEGER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  def var wLoop as int no-undo.
      
  DEF BUFFER bufArtBas FOR ArtBas.

  FINN-NESTE:  
  repeat wLoop = 1 to 10000:
  
    if wLoop = 0 then
      next FINN-NESTE.
      
    if can-find(bufArtBas no-lock where
      bufArtBas.Vg    = ArtBas.Vg and
      bufArtBas.LopNr = wLoop) then
      do:
        next FINN-NESTE.
      end.
    else
      leave FINN-NESTE.          
  end. /* FINN-NESTE */
  
  if wLoop > 9999 then
      RETURN ?.
  ELSE
      RETURN wLoop.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

