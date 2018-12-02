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

DEF INPUT  PARAMETER fArtikkelNr AS DEC   NO-UNDO.
DEF INPUT  PARAMETER fVareBokNr  AS DEC   NO-UNDO.
DEF OUTPUT PARAMETER rRowIdLinje AS ROWID NO-UNDO.

DEF VAR iCl               AS INT NO-UNDO.
DEF VAR hBuffVarebokLinje AS HANDLE NO-UNDO.

DEF BUFFER clButiker FOR Butiker.

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
FIND clButiker WHERE clButiker.Butik = iCL NO-LOCK NO-ERROR.
IF NOT AVAIL clButiker THEN DO:
  RETURN "Finner ikke sentral-lager: " + STRING(iCL).
END.

RUN nyLinje.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-nyLinje) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE nyLinje Procedure 
PROCEDURE nyLinje :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /* Sjekker om artikkelen ligger på vareboken fra før. */
  IF CAN-FIND(VareBokLinje WHERE
              VareBokLinje.VareBokNr = fVareBokNr AND
              VareBokLinje.ArtikkelNr = fArtikkelNr) THEN
  DO:
      rRowIdLinje = ?.
      RETURN "Artikkel er allerede lagt inn i vareboken.".
  END.

  FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = fArtikkelNr NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
  DO:
      rRowIdLinje = ?.
      RETURN "Ukjent artikkel.".
  END.
  IF ArtBas.LopNr = ? OR 
     /*ArtBas.pakke = TRUE OR*/
     ArtBas.OPris = TRUE OR
     ArtBas.Pant  = TRUE
      THEN 
  DO:
      rRowIdLinje = ?.
      /*RETURN "Pakke, PLU, Pant og artikler uten løpenummer, kan ikke legges inn i varebok.".*/
      RETURN "PLU, Pant og artikler uten løpenummer, kan ikke legges inn i varebok.".
  END.

  FIND VareBokHode NO-LOCK WHERE
      VareBokHode.VareBokNr = fVareBokNr NO-ERROR.

  /* Henter pris. ALLTID normalpris. */
  FIND ArtPris NO-LOCK WHERE
       ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
       ArtPris.ProfilNr   = VareBokHode.ProfilNr NO-ERROR.

  /* Henter pris fra sentrallageret hvis den ikke finnes for butikken. */
  IF NOT AVAILABLE ArtPris THEN
    FIND ArtPris NO-LOCK WHERE
         ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
         ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
  IF NOT AVAILABLE ArtPris THEN
      RETURN "Ingen pris på artikkelen.".

  FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
  IF NOT AVAILABLE VarGr THEN
      RETURN "Ukjent varegruppe på artikkelen.".
  FIND HuvGr NO-LOCK WHERE
      HuvGr.Hg = ArtBas.Hg NO-ERROR.
  IF NOT AVAILABLE HuvGr THEN
      RETURN "Ukjent hovedgruppe på artikkelen.".
  FIND Produsent OF ArtBas NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Produsent THEN
      RETURN "Unkjent produsent på artikkelen.".
  FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
  IF NOT AVAILABLE LevBas THEN
      RETURN "Unkjent leverandør på artikkelen.".
  FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Avdeling THEN
      RETURN "Ukjent avdeling på artikkelens varegruppe.".
                                         
  DO TRANSACTION:
      CREATE VareBokLinje.
      BUFFER-COPY ArtBas TO VareBokLinje
          ASSIGN
          VareBokLinje.VareBokNr    = fVareBokNr
          VareBokLinje.AvdelingNr   = (IF AVAILABLE HuvGr
                                        THEN HuvGr.AvdelingNr
                                        ELSE 0)                                        
          VareBokLinje.LevDato1     = ArtBas.LevDato1
          VareBokLinje.LevDato2     = ArtBas.LevDato2
          VareBokLinje.LevDato3     = ArtBas.LevDato3
          VareBokLinje.LevDato4     = ArtBas.LevDato4
          VareBokLinje.ProdNr       = ArtBas.ProdNr
          VareBokLinje.ProdusentBeskrivelse = Produsent.Beskrivelse
          VareBokLinje.VgBeskr      = VarGr.VgBeskr
          VareBokLinje.HgBeskr      = HuvGr.HgBeskr
          VareBokLinje.LevNamn      = LevBas.LevNamn
          VareBokLinje.LinjeMerknad = ArtBas.LinjeMerknad
          VareBokLinje.AvdelingNavn = Avdeling.AvdelingNavn
          Vareboklinje.Gjennomfaktureres = TRUE
          .
      ASSIGN
          rRowIdLinje = ROWID(VareBokLinje)
          .
      IF AVAILABLE ArtPris THEN
          ASSIGN
/*           VareBokLinje.DBKr         = ArtPris.DBKr[1] */
/*           VareBokLinje.VareKost     = ArtPris.VareKost[1] */
          VareBokLinje.InnkjopsPris = ArtBas.KatalogPris  
/*         VareBokLinje.InnkjopsPris = ArtPris.InnkjopsPris[1] */
/*           VareBokLinje.DB%          = ArtPris.DB%[1] */
          VareBokLinje.Mva%         = ArtPris.Mva%[1]          
          VareBokLinje.Pris         = ArtBas.AnbefaltPris
/*           VareBokLinje.Pris         = ArtPris.Pris[1]   */
/*           VareBokLinje.forhRab%     = ArtPris.Rab1%[1]  */
/*           VareBokLinje.Rab1Kr       = ArtPris.Rab1Kr[1] */
          .
      ASSIGN
/*           VareBokLinje.supDBKr         = VareBokLinje.DBKr */
/*           VareBokLinje.supVareKost     = VareBokLinje.VareKost */
          VareBokLinje.supInnkjopsPris = VareBokLinje.InnkjopsPris
/*           VareBokLinje.supDB%          = VareBokLinje.DB% */
          VareBokLinje.supPris         = VareBokLinje.Pris        
/*           VareBokLinje.supRab%         = VareBokLinje.forhRab% */
/*           VareBokLinje.supRab1Kr       = VareBokLinje.Rab1Kr   */
          hBuffVarebokLinje            = BUFFER VarebokLinje:HANDLE
          .
      RUN vareboklinje_kalkuler.p (hBuffVarebokLinje,"forhRab%").
      RUN vareboklinje_kalkuler.p (hBuffVarebokLinje,"supRab%").
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

