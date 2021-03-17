&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : pfxoppdatfastereg.p
    Purpose     : Integrasjon ProfitBase
                  Oppdatering av faste registre som benyttes av ProfitBase.

    Syntax      :

    Description : Rutinen tar alltid alle registre. Startes automatisk 
                  etter import av VPI fra MegaDisc på HK.

    Author(s)   : Tom Nøkleby
    Created     : 18/3-03
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF VAR cRutiner AS CHAR NO-UNDO.
DEF VAR cRutNavn AS CHAR NO-UNDO.
DEF VAR iLoop1      AS INT  NO-UNDO.
DEF VAR cTrans      AS CHAR NO-UNDO.
DEF VAR cNoTrans    AS CHAR NO-UNDO.
DEF VAR iAntNye     AS INT  NO-UNDO.
DEF VAR iAntEndret  AS INT  NO-UNDO.
DEF VAR iAntSlettet AS INT  NO-UNDO.

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

RUN reControll.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-reControll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reControll Procedure 
PROCEDURE reControll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Liste over rutiner som skal kjøres. */
ASSIGN
  cTrans   = "reDep,reItGrp,reItSGrp,reVend,reMane,reSl,reMed"
  cNoTrans = "reItInf"
  .
/* Starter oppdateringsrutiner */
OPPDATER-TRANS:
DO iLoop1 = 1 TO NUM-ENTRIES(cTrans) TRANSACTION:
  ASSIGN
    iAntNye     = 0
    iAntEndret  = 0
    iAntSlettet = 0
    .
  PUBLISH 'PBR' (string(today) + " " + string(time,"HH:MM:SS") + " " +
                 "PRB Starter rutine " + ENTRY(iLoop1,cTrans) + ","). 
  RUN VALUE(ENTRY(iLoop1,cTrans)).
  PUBLISH 'PBR' (string(today) + " " + string(time,"HH:MM:SS") + " " + cRutNavn + " " +
                 "PRB Nye: " + string(iAntNye) + 
                 " Endret: " + STRING(iAntEndret) +
                 " Slettet: " + STRING(iAntSlettet) + "."). 
END. /* OPPDATER-TRANS */

OPPDATER-NOTRANS:
DO iLoop1 = 1 TO NUM-ENTRIES(cNoTrans):
  ASSIGN
    iAntNye     = 0
    iAntEndret  = 0
    iAntSlettet = 0
    .
  PUBLISH 'PBR' (string(today) + " " + string(time,"HH:MM:SS") + " " +
                 "PRB Starter rutine " + ENTRY(iLoop1,cNoTrans) + ","). 
  RUN VALUE(ENTRY(iLoop1,cNoTrans)).
  PUBLISH 'PBR' (string(today) + " " + string(time,"HH:MM:SS") + " " + cRutNavn + " " +
                 "PRB Nye: " + string(iAntNye) + 
                 " Endret: " + STRING(iAntEndret) +
                 " Slettet: " + STRING(iAntSlettet) + "."). 
END. /* OPPDATER-NOTRANS */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-reDep) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reDep Procedure 
PROCEDURE reDep :
/*------------------------------------------------------------------------------
  Purpose:     Oppdaterer Avdelinger.
  Parameters:  <none>
  Notes:       Ekstern transaksjonshåndtering.
------------------------------------------------------------------------------*/
  ASSIGN
    cRutNavn = "Avdeling"
    .

  /* Flagger først alle postene som slettet. */
  SLETTET:
  FOR EACH pfDepartments EXCLUSIVE-LOCK:
    ASSIGN
      pfDepartments.slettet     = TRUE
      pfDepartments.SlettetDato = TODAY
      .
  END. /* SLETTET */

  /* Oppdaterer alle poster pånytt. */
  OPPDATER:
  FOR EACH Avdeling NO-LOCK:
    FIND pfDepartments EXCLUSIVE-LOCK WHERE
      pfDepartments.DepartmentId = Avdeling.AvdelingNr NO-ERROR.
    IF NOT AVAILABLE pfDepartments THEN
    DO:
      CREATE pfDepartments.
      ASSIGN
        iAntNye                    = iAntnye + 1
        pfDepartments.DepartmentId = Avdeling.AvdelingNr
        .
    END.
    ELSE
      ASSIGN
        iAntEndret = iAntendret + 1
        .

    ASSIGN
      pfDepartments.DESCRIPTION = Avdeling.AvdelingNavn
      pfDepartments.Slettet     = FALSE
      pfDepartments.SlettetDato = ?
      .
  END. /* OPPDATER */

  /* Teller opp antall slettede poster. */
  FOR EACH pfDepartments NO-LOCK:
    IF pfDepartments.Slettet = TRUE THEN
      iAntSlettet = iAntSlettet + 1.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-reItGrp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reItGrp Procedure 
PROCEDURE reItGrp :
/*------------------------------------------------------------------------------
  Purpose:     Oppdaterer hovedgrupper.
  Parameters:  <none>
  Notes:       Ekstern transaksjonshåndtering.
------------------------------------------------------------------------------*/
  ASSIGN
    cRutNavn = "Hovedgrupper"
    .

  /* Flagger først alle postene som slettet. */
  SLETTET:
  FOR EACH pfItemGroup EXCLUSIVE-LOCK:
    ASSIGN
      pfItemGroup.slettet     = TRUE
      pfItemGroup.SlettetDato = TODAY
      .
  END. /* SLETTET */

  /* Oppdaterer alle poster pånytt. */
  OPPDATER:
  FOR EACH HuvGr NO-LOCK:
    FIND pfItemGroup EXCLUSIVE-LOCK WHERE
      pfItemGroup.GroupId = HuvGr.Hg NO-ERROR.
    IF NOT AVAILABLE pfItemGroup THEN
    DO:
      CREATE pfItemGroup.
      ASSIGN
        iAntNye             = iAntnye + 1
        pfItemGroup.GroupId = HuvGr.Hg
        .
    END.
    ELSE
      ASSIGN
        iAntEndret = iAntendret + 1
        .

    ASSIGN
      pfItemGroup.DESCRIPTION = HuvGr.HgBeskr
      pfItemGroup.Slettet     = FALSE
      pfItemGroup.SlettetDato = ?
      .
  END. /* OPPDATER */

  /* Teller opp antall slettede poster. */
  FOR EACH pfItemGroup NO-LOCK:
    IF pfItemGroup.Slettet = TRUE THEN
      iAntSlettet = iAntSlettet + 1.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-reItInf) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reItInf Procedure 
PROCEDURE reItInf :
/*------------------------------------------------------------------------------
  Purpose:     Oppdaterer vareinformasjon.
  Parameters:  <none>
  Notes:       Ekstern transaksjonshåndtering.
  
  Ref. Nerland i E-Mail
  Vi har glemt å få med mva-koder. I Storepoint ligger dette i egen tabell, se
  vedlegg, jeg foreslår imidlertid at vi legger dette i  pfiteminfo.

  Vi trenger å legge inn følgende felt iVat2Id, iVat3Id, iVat4Id og iVat5Id,
  hvor Vat-kodene her står for hhv. 'Fri Oms', 'Mva 0%', 'Mva 24%' og 'Mva
  12%'.

Regner med at 4 mva-koder holder?

------------------------------------------------------------------------------*/
  DEF VAR piMvaKod AS INT NO-UNDO.
  ASSIGN
    cRutNavn = "Vareinformasjon"
    .

  /* Flagger først alle postene som slettet. */
  SLETTET:
  FOR EACH pfItemInfo EXCLUSIVE-LOCK:
    ASSIGN
      pfItemInfo.slettet     = TRUE
      pfItemInfo.SlettetDato = TODAY
      .
  END. /* SLETTET */

  /* Oppdaterer alle poster pånytt. */
  OPPDATER:
  FOR EACH Strekkode NO-LOCK:
    FIND ArtBas OF Strekkode NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ArtBas THEN
      NEXT OPPDATER.
    FIND HuvGr OF ArtBas NO-LOCK no-error.
    IF AVAILABLE HuvGr THEN
        FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE VarGr THEN
        FIND Moms OF VarGr   NO-LOCK NO-ERROR.

    IF AVAILABLE Moms THEN
        piMvaKod = Moms.MomsKod.
    ELSE
        piMvaKod = 1.

    FIND pfItemInfo EXCLUSIVE-LOCK WHERE
      pfItemInfo.MainItemId = Strekkode.Kode NO-ERROR.
    IF NOT AVAILABLE pfItemInfo THEN
    DO:
      CREATE pfItemInfo.
      ASSIGN
        iAntNye                    = iAntnye + 1
        pfItemInfo.MainItemId      = Strekkode.Kode
        pfItemInfo.ItemInternalKey = Strekkode.ArtikkelNr
        .
    END.
    ELSE
      ASSIGN
        iAntEndret = iAntendret + 1
        .

    ASSIGN
      pfItemInfo.FullName      = ArtBas.Beskr
      pfItemInfo.SubGroupId    = ArtBas.Vg
      pfItemInfo.GroupId       = ArtBas.Hg
      pfItemInfo.DepartmentId  = IF AVAILABLE HuvGr
                                   THEN HuvGr.AvdelingNr
                                   ELSE 0
      pfItemInfo.MainVendorId  = ArtBas.LevNr
      pfItemInfo.ManuFactureId = ArtBas.ProdNr
      pfItemInfo.Slettet       = FALSE
      pfItemInfo.SlettetDato   = ?
      .
    CASE piMvaKod:
        WHEN 7 THEN pfItemInfo.Vat1Id = 1. /*  6%  */
        WHEN 4 THEN pfItemInfo.Vat2Id = 1. /* Fri  */
        WHEN 0 THEN pfItemInfo.Vat3Id = 1. /*  0%  */
        WHEN 1 THEN pfItemInfo.Vat4Id = 1. /* 25%  */
        WHEN 6 THEN pfItemInfo.Vat5Id = 1. /* 12%  */
    END CASE.
  END. /* OPPDATER */

  /* Teller opp antall slettede poster. */
  FOR EACH pfItemInfo NO-LOCK:
    IF pfItemInfo.Slettet = TRUE THEN
      iAntSlettet = iAntSlettet + 1.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-reItSGrp) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reItSGrp Procedure 
PROCEDURE reItSGrp :
/*------------------------------------------------------------------------------
  Purpose:     Oppdaterer varegrupper.
  Parameters:  <none>
  Notes:       Ekstern transaksjonshåndtering.
------------------------------------------------------------------------------*/
  ASSIGN
    cRutNavn = "Varegrupper"
    .

  /* Flagger først alle postene som slettet. */
  SLETTET:
  FOR EACH pfItemSubGroup EXCLUSIVE-LOCK:
    ASSIGN
      pfItemSubGroup.slettet     = TRUE
      pfItemSubGroup.SlettetDato = TODAY
      .
  END. /* SLETTET */

  /* Oppdaterer alle poster pånytt. */
  OPPDATER:
  FOR EACH VarGr NO-LOCK:
    FIND pfItemSubGroup EXCLUSIVE-LOCK WHERE
      pfItemSubGroup.SubGroupId = string(VarGr.Vg) NO-ERROR.
    IF NOT AVAILABLE pfItemSubGroup THEN
    DO:
      CREATE pfItemSubGroup.
      ASSIGN
        iAntNye             = iAntnye + 1
        pfItemSubGroup.SubGroupId = string(VarGr.Vg)
        .
    END.
    ELSE
      ASSIGN
        iAntEndret = iAntendret + 1
        .

    ASSIGN
      pfItemSubGroup.DESCRIPTION = VarGr.VgBeskr
      pfItemSubGroup.Slettet     = FALSE
      pfItemSubGroup.SlettetDato = ?
      .
  END. /* OPPDATER */

  /* Teller opp antall slettede poster. */
  FOR EACH pfItemSubGroup NO-LOCK:
    IF pfItemSubGroup.Slettet = TRUE THEN
      iAntSlettet = iAntSlettet + 1.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-reMane) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reMane Procedure 
PROCEDURE reMane :
/*------------------------------------------------------------------------------
  Purpose:     Oppdaterer produsenter.
  Parameters:  <none>
  Notes:       Ekstern transaksjonshåndtering.
------------------------------------------------------------------------------*/
  ASSIGN
    cRutNavn = "Produsenter"
    .

  /* Flagger først alle postene som slettet. */
  SLETTET:
  FOR EACH pfManefacturer EXCLUSIVE-LOCK:
    ASSIGN
      pfManefacturer.slettet     = TRUE
      pfManefacturer.SlettetDato = TODAY
      .
  END. /* SLETTET */

  /* Oppdaterer alle poster pånytt. */
  OPPDATER:
  FOR EACH Produsent NO-LOCK:
    FIND pfManefacturer EXCLUSIVE-LOCK WHERE
      pfManefacturer.ManuFactureId = dec(Produsent.ProdNr) NO-ERROR.
    IF NOT AVAILABLE pfManefacturer THEN
    DO:
      CREATE pfManefacturer.
      ASSIGN
        iAntNye             = iAntnye + 1
        pfManefacturer.ManuFactureId = dec(Produsent.ProdNr)
        .
    END.
    ELSE
      ASSIGN
        iAntEndret = iAntendret + 1
        .

    ASSIGN
      pfManefacturer.NAME        = Produsent.Beskrivelse
      pfManefacturer.Slettet     = FALSE
      pfManefacturer.SlettetDato = ?
      .
  END. /* OPPDATER */

  /* Teller opp antall slettede poster. */
  FOR EACH pfManefacturer NO-LOCK:
    IF pfManefacturer.Slettet = TRUE THEN
      iAntSlettet = iAntSlettet + 1.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-reMed) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reMed Procedure 
PROCEDURE reMed :
/*------------------------------------------------------------------------------
  Purpose:     Oppdaterer transaksjonstyper.
  Parameters:  <none>
  Notes:       
  Notes:       Ekstern transaksjonshåndtering.
------------------------------------------------------------------------------*/
  ASSIGN
    cRutNavn = "Transaksjonstyper"
    .

  DEF VAR piMediaId   AS INT  NO-UNDO.     
  DEF VAR pcPOSKoder  AS CHAR NO-UNDO.
  DEF VAR pcTTIdKoder AS CHAR NO-UNDO.
  DEF VAR pcPOS       AS CHAR NO-UNDO.
  DEF VAR piEntry     AS INT  NO-UNDO.
  DEF VAR pcTTId      AS CHAR NO-UNDO.

/*   ASSIGN                                                                                                                                 */
/*       pcPOSKoder      = "001,003,xxx,xxx,xxx,xxx,xxx,xxx,xxx,004,xxx,005,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,006,xxx,xxx,xxx,xxx,xxx," + */
/*                         "xxx,xxx,xxx,xxx,xxx,011,xxx,012,014,xxx,xxx,xxx,xxx,xxx,xxx,xxx," +                                             */
/*                         "xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx," +                         */
/*                         "xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx," +                     */
/*                         "xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,xxx,007"                                                                        */
/*                                                                                                                                          */
/*       pcTTIdKoder     = "001,002,003,004,005,006,007,008,009,010,011,012,013,014,015,016,017,018,019,022,023,149,050,079,051,052,053," + */
/*                         "054,055,056,057,058,059,060,061,062,063,064,065,066,067,068,069," +                                             */
/*                         "070,071,072,073,080,081,082,086,087,088,089,090,091,092,093,094,095,096,097,098,099," +                         */
/*                         "100,101,102,103,104,105,106,107,108,120,121,122,123,130,131,140,141,142,143,144,145,146," +                     */
/*                         "200,201,202,203,147,150,132,135,134,203"                                                                        */
/*     .                                                                                                                                    */

                             
  /* Flagger først alle postene som slettet. */
  SLETTET:
  FOR EACH pfMedias EXCLUSIVE-LOCK:
    ASSIGN
      pfMedias.slettet     = TRUE
      pfMedias.SlettetDato = TODAY
      .
  END. /* SLETTET */

  /* Oppdaterer alle poster pånytt. */
  OPPDATER:
  FOR EACH Transtype NO-LOCK:

/*     /* Konverter mediaId */                                        */
/*     KONVERTER:                                                     */
/*     DO:                                                            */
/*       /* Konverterer fra SkoTex BackOffice kode til ProfitBase. */ */
/*       ASSIGN                                                       */
/*         pcTTId = trim(STRING(Transtype.TTId,">999"))               */
/*         .                                                          */
/*       /* Konverterer transaksjonskoden */                          */
/*       KONVTRANSKODE:                                               */
/*       DO:                                                          */
/*           ASSIGN                                                   */
/*             piEntry     = LOOKUP(pcTTId,pcTTIdKoder)               */
/*             piMediaId = ?                                          */
/*             .                                                      */
/*           IF piEntry = 0 THEN                                      */
/*               piMediaId = ?.                                       */
/*           ELSE DO:                                                 */
/*             IF ENTRY(piEntry,pcPOSKoder) = "xxx" THEN              */
/*               NEXT OPPDATER.                                       */
/*             ASSIGN                                                 */
/*               piMediaId = INT(ENTRY(piEntry,pcPOSKoder))           */
/*               NO-ERROR.                                            */
/*           END.                                                     */
/*       END. /* KONVTRANSKODE */                                     */
/*                                                                    */
/*       IF piMediaId = ? THEN                                        */
/*         NEXT OPPDATER.                                             */
/*     END. /* KONVERTER */                                           */
    ASSIGN
      piMediaId = TransType.TTId
      .

    FIND pfMedias EXCLUSIVE-LOCK WHERE
      pfMedias.MediaId = piMediaId NO-ERROR.
    IF NOT AVAILABLE pfMedias THEN
    DO:
      CREATE pfMedias.
      ASSIGN
        iAntNye            = iAntnye + 1
        pfMedias.MediaId = piMediaId
        .
    END.
    ELSE
      ASSIGN
        iAntEndret = iAntendret + 1
        .

    ASSIGN
      pfMedias.DESCRIPTION = Transtype.Beskrivelse
      pfMedias.Slettet     = FALSE
      pfMedias.SlettetDato = ?
      .
  END. /* OPPDATER */

  /* Teller opp antall slettede poster. */
  FOR EACH pfMedias NO-LOCK:
    IF pfMedias.Slettet = TRUE THEN
      iAntSlettet = iAntSlettet + 1.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-reSl) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reSl Procedure 
PROCEDURE reSl :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       Ekstern transaksjonshåndtering.
------------------------------------------------------------------------------*/
  DEF VAR pDato1      AS DATE NO-UNDO.
  DEF VAR pDato2      AS DATE NO-UNDO.

  ASSIGN
    cRutNavn = "Butikkregister"
    .

  /* Flagger først alle postene som slettet. */
  SLETTET:
  FOR EACH pfSlinfo EXCLUSIVE-LOCK:
    ASSIGN
      pfSlinfo.slettet     = TRUE
      pfSlinfo.SlettetDato = TODAY
      .
  END. /* SLETTET */

  /* Oppdaterer alle poster pånytt. */
  OPPDATER:
  FOR EACH Butiker NO-LOCK:
    FIND KjedensButikker NO-LOCK WHERE
      Kjedensbutikker.ButikkNr = Butiker.butik NO-ERROR.
    IF NOT AVAIL Kjedensbutikker THEN
        NEXT.
    FIND KjedeRegion NO-LOCK where
      KjedeRegion.KjedeNr  = Kjedensbutikker.KjedeNr AND
      KjedeRegion.RegionNr = Kjedensbutikker.RegionNr NO-ERROR.
    FIND KjedeDistrikt NO-LOCK where
      KjedeDistrikt.KjedeNr    = Kjedensbutikker.KjedeNr AND
      KjedeDistrikt.DistriktNr = Kjedensbutikker.DistriktNr NO-ERROR.
    IF AVAIL KjedensButikker THEN
        FIND DriftsForm OF KjedensButikker.
    ELSE
        RELEASE DriftsForm.
/*     FIND SegmentRegister OF Butiker NO-LOCK NO-ERROR. */

    FIND pfSlinfo EXCLUSIVE-LOCK WHERE
      pfSlinfo.SalgL_Nr = (Butiker.Butik) NO-ERROR.
    IF NOT AVAILABLE pfSlinfo THEN
    DO:
      CREATE pfSlinfo.
      ASSIGN
        iAntNye             = iAntnye + 1
        pfSlinfo.SalgL_Nr = (Butiker.Butik)
        .
    END.
    ELSE
      ASSIGN
        iAntEndret = iAntendret + 1
        .

    /* Sjekker for midlertidig stengt. */
    RUN sjekktempstengt.p (Butiker.Butik, TODAY, OUTPUT pDato1, OUTPUT pDato2).

    ASSIGN
      pfSlinfo.SalgsL_Navn        = Butiker.ButNamn
      pfSlinfo.ApnetDato          = int(
                                        string(year(Butiker.ApningsDato),"9999") + 
                                        string(month(Butiker.ApningsDato),"99") +
                                        string(day(Butiker.ApningsDato),"99") 
                                        )
      pfSlInfo.NedlagtDato        = int(
                                        string(year(Butiker.NedlagtDato),"9999") + 
                                        string(month(Butiker.NedlagtDato),"99") +
                                        string(day(Butiker.NedlagtDato),"99") 
                                        )
      pfSlinfo.Slettet            = FALSE
      pfSlinfo.SlettetDato        = ?

    pfSlInfo.SegmentKode         = IF AVAILABLE DriftsForm
                                    THEN STRING(DriftsForm.DriftsFormId)
                                    ELSE ""
    pfslInfo.SegmentBeskrivelse = IF AVAILABLE DriftsForm
                                    THEN DriftsForm.DriftsFormNavn
                                    ELSE ""
/*       pfSlInfo.SegmentKode        = IF AVAILABLE SegmentRegister              */
/*                                       THEN SegmentRegister.SegmentKNavn       */
/*                                       ELSE ""                                 */
/*       pfslInfo.SegmentBeskrivelse = IF AVAILABLE SegmentRegister              */
/*                                       THEN SegmentRegister.SegmentBeskrivelse */
/*                                       ELSE ""                                 */
      pfSlInfo.Region             = IF AVAILABLE Kjedensbutikker
                                      THEN Kjedensbutikker.RegionNr
                                      ELSE 0
      pfSlinfo.RegionNavn         = IF AVAILABLE KjedeRegion 
                                      THEN KjedeRegion.RegionNavn
                                      ELSE ""
      pfSlinfo.distriktsNr        = IF AVAILABLE Kjedensbutikker
                                      THEN Kjedensbutikker.DistriktNr
                                      ELSE 0
      /* info er hentet fra åpningsskjema for butikken. */
      pfSlInfo.MidlStengtFra      = int(
                                        string(year(pDato1),"9999") + 
                                        string(month(pDato1),"99") +
                                        string(day(pDato1),"99") 
                                        )
      pfSlInfo.MidlStengtTil      = int(
                                        string(year(pDato2),"9999") + 
                                        string(month(pDato2),"99") +
                                        string(day(pDato2),"99") 
                                        )
      .
  END. /* OPPDATER */

  /* Teller opp antall slettede poster. */
  FOR EACH pfSlinfo WHERE pfSlinfo.Slettet = TRUE:
      iAntSlettet = iAntSlettet + 1.
      DELETE pfSlinfo.
  END.
/*   FOR EACH pfSlinfo NO-LOCK:         */
/*     IF pfSlinfo.Slettet = TRUE THEN  */
/*       iAntSlettet = iAntSlettet + 1. */
/*   END.                               */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-reVend) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE reVend Procedure 
PROCEDURE reVend :
/*------------------------------------------------------------------------------
  Purpose:     Oppdaterer leverandører.
  Parameters:  <none>
  Notes:       Ekstern transaksjonshåndtering.
------------------------------------------------------------------------------*/
  ASSIGN
    cRutNavn = "Leverandørregister"
    .

  /* Flagger først alle postene som slettet. */
  SLETTET:
  FOR EACH pbVendor EXCLUSIVE-LOCK:
    ASSIGN
      pbVendor.slettet     = TRUE
      pbVendor.SlettetDato = TODAY
      .
  END. /* SLETTET */

  /* Oppdaterer alle poster pånytt. */
  OPPDATER:
  FOR EACH LevBas NO-LOCK:
    FIND pbVendor EXCLUSIVE-LOCK WHERE
      pbVendor.VendorId = dec(LevBas.LevNr) NO-ERROR.
    IF NOT AVAILABLE pbVendor THEN
    DO:
      CREATE pbVendor.
      ASSIGN
        iAntNye             = iAntnye + 1
        pbVendor.VendorId = dec(LevBas.LevNr)
        .
    END.
    ELSE
      ASSIGN
        iAntEndret = iAntendret + 1
        .

    ASSIGN
      pbVendor.NAME        = LevBas.LevNamn
      pbVendor.Slettet     = FALSE
      pbVendor.SlettetDato = ?
      .
  END. /* OPPDATER */

  /* Teller opp antall slettede poster. */
  FOR EACH pbVendor NO-LOCK:
    IF pbVendor.Slettet = TRUE THEN
      iAntSlettet = iAntSlettet + 1.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

