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
DEF INPUT  PARAMETER fVarebehNr  AS DEC   NO-UNDO.
DEF OUTPUT PARAMETER rRowIdLinje AS ROWID NO-UNDO.

DEF VAR iCl               AS INT NO-UNDO.
DEF VAR hBuffVarebehLinje AS HANDLE NO-UNDO.

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
  /* Sjekker om artikkelen ligger på Varebehen fra før. */
  IF CAN-FIND(VarebehLinje WHERE
              VarebehLinje.VarebehNr = fVarebehNr AND
              VarebehLinje.ArtikkelNr = fArtikkelNr) THEN
  DO:
      rRowIdLinje = ?.
      RETURN "Artikkel er allerede lagt inn i Varebehen.".
  END.

  FIND ArtBas NO-LOCK WHERE
      ArtBas.ArtikkelNr = fArtikkelNr NO-ERROR.
  IF NOT AVAILABLE ArtBas THEN
  DO:
      rRowIdLinje = ?.
      RETURN "Ukjent artikkel.".
  END.
  IF 
     ArtBas.LopNr = ? OR
     /*ArtBas.pakke = TRUE OR*/
     ArtBas.OPris = TRUE OR
     ArtBas.Pant  = TRUE
      THEN 
  DO:
      rRowIdLinje = ?.
      RETURN "PLU, Pant og artikler uten løpenummer, kan ikke legges inn i Varebeh.".
  END.

  FIND VarebehHode NO-LOCK WHERE
      VarebehHode.VarebehNr = fVarebehNr NO-ERROR.

  /* Henter pris. ALLTID normalpris. */
  FIND ArtPris NO-LOCK WHERE
       ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
       ArtPris.ProfilNr   = VarebehHode.ProfilNr NO-ERROR.

  /* Henter pris fra sentrallageret hvis den ikke finnes for butikken. */
  IF NOT AVAILABLE ArtPris THEN
    FIND ArtPris NO-LOCK WHERE
         ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
         ArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
/*   IF NOT AVAILABLE ArtPris THEN           */
/*       RETURN "Ingen pris på artikkelen.". */

  FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
  IF NOT AVAILABLE VarGr THEN
      RETURN "Ukjent varegruppe på artikkelen.".
  FIND HuvGr NO-LOCK WHERE
      HuvGr.Hg = ArtBas.Hg NO-ERROR.
  IF NOT AVAILABLE HuvGr THEN
      RETURN "Ukjent hovedgruppe på artikkelen.".
  FIND Produsent OF ArtBas NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Produsent THEN
      RETURN "Ukjent produsent på artikkelen.".
  FIND LevBas OF ArtBas NO-LOCK NO-ERROR.
  IF NOT AVAILABLE LevBas THEN
      RETURN "Ukjent leverandør på artikkelen.".
  FIND Avdeling OF HuvGr NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Avdeling THEN
      RETURN "Ukjent avdeling på artikkelens varegruppe.".
                                         
  DO TRANSACTION:
      CREATE VarebehLinje.
      BUFFER-COPY ArtBas TO VarebehLinje
          ASSIGN
          VarebehLinje.VarebehNr    = fVarebehNr
          VarebehLinje.AvdelingNr   = (IF AVAILABLE HuvGr
                                        THEN HuvGr.AvdelingNr
                                        ELSE 0)                                        
          VarebehLinje.LevDato1     = ArtBas.LevDato1
          VarebehLinje.LevDato2     = ArtBas.LevDato2
          VarebehLinje.LevDato3     = ArtBas.LevDato3
          VarebehLinje.LevDato4     = ArtBas.LevDato4
          VarebehLinje.ProdNr       = ArtBas.ProdNr
          VarebehLinje.ProdusentBeskrivelse = Produsent.Beskrivelse
          VarebehLinje.VgBeskr      = VarGr.VgBeskr
          VarebehLinje.HgBeskr      = HuvGr.HgBeskr
          VarebehLinje.LevNamn      = LevBas.LevNamn
          VarebehLinje.LinjeMerknad = ArtBas.LinjeMerknad
          VarebehLinje.AvdelingNavn = Avdeling.AvdelingNavn

          VarebehLinje.InnkjopsPris = ArtBas.KatalogPris  
          VarebehLinje.Pris         = ArtBas.Pris
          .
      ASSIGN
          rRowIdLinje = ROWID(VarebehLinje)
          .
      IF AVAILABLE ArtPris THEN
          ASSIGN
/*           VarebehLinje.DBKr         = ArtPris.DBKr[1] */
          VarebehLinje.VareKost     = ArtPris.VareKost[1]
/*           VarebehLinje.InnkjopsPris = ArtPris.VareKost[1]  */
          VarebehLinje.Pris         = ArtPris.Pris[1]
          VarebehLinje.InnkjopsPris = ArtPris.InnkjopsPris[1]
          VarebehLinje.DB%          = ArtPris.DB%[1]
          VarebehLinje.DBkr         = ArtPris.DBkr[1]
          VarebehLinje.Mva%         = ArtPris.Mva%[1]          
/*           VarebehLinje.Pris         = ArtPris.Pris[1]   */
/*           VarebehLinje.forhRab%     = ArtPris.Rab1%[1]  */
/*           VarebehLinje.Rab1Kr       = ArtPris.Rab1Kr[1] */
          .
      ASSIGN
/*           VarebehLinje.supDBKr         = VarebehLinje.DBKr */
/*           VarebehLinje.supVareKost     = VarebehLinje.VareKost */
          VarebehLinje.supInnkjopsPris = VarebehLinje.InnkjopsPris
/*           VarebehLinje.supDB%          = VarebehLinje.DB% */
          VarebehLinje.supPris         = VarebehLinje.Pris        
/*           VarebehLinje.supRab%         = VarebehLinje.forhRab% */
/*           VarebehLinje.supRab1Kr       = VarebehLinje.Rab1Kr   */
          hBuffVarebehLinje            = BUFFER VarebehLinje:HANDLE
          .
/*       RUN Varebehlinje_kalkuler.p (hBuffVarebehLinje,"forhRab%").  */
/*       RUN Varebehlinje_kalkuler.p (hBuffVarebehLinje,"supRab%").   */
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

