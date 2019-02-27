&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
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
  DEFINE INPUT  PARAMETER iButikknr  AS INTEGER     NO-UNDO.
  DEFINE INPUT  PARAMETER cDirection AS CHARACTER   NO-UNDO.   /* C=Current, P=Previous, N=Next */
  DEFINE INPUT  PARAMETER cBrukerId AS CHARACTER   NO-UNDO.
  DEFINE INPUT-OUTPUT  PARAMETER dDato     AS DATE    NO-UNDO.
  DEFINE OUTPUT PARAMETER cLayout AS CHAR NO-UNDO. 
  DEFINE OUTPUT PARAMETER DATASET-HANDLE hDset.
  
  DEFINE VARIABLE cCustLayout AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE dNetRab      AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE iCount       AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAntkunder   AS INTEGER    NO-UNDO.
  DEFINE VARIABLE iAntkvitt    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE dDBKrtmp     AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dSumDBKrtmp  AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE dSumVerdiSolgt AS DECIMAL    NO-UNDO.
  DEFINE VARIABLE cTekst       AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE iSumButNr    AS INTEGER    NO-UNDO.
  DEFINE VARIABLE bOk          AS LOG  NO-UNDO.
  
  DEFINE VARIABLE iWdOff     AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cDayString AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dLokDato AS DATE NO-UNDO.

  DEFINE VARIABLE iAarPerLinNr AS INTEGER    NO-UNDO.
  DEFINE VARIABLE cButListe    AS CHARACTER  NO-UNDO.
  DEFINE VARIABLE cTillgbutiker AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cStatType AS CHARACTER   NO-UNDO.

  DEFINE VARIABLE httAvdeling AS HANDLE      NO-UNDO.
  DEFINE VARIABLE cAvdFelter AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cAvdLabels AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE httHuvGr    AS HANDLE      NO-UNDO.
  DEFINE VARIABLE httVg       AS HANDLE      NO-UNDO.
  DEFINE VARIABLE httLev      AS HANDLE      NO-UNDO.
  DEFINE VARIABLE httSelger   AS HANDLE      NO-UNDO.
  DEFINE VARIABLE httakt_rapp AS HANDLE      NO-UNDO.
  DEFINE VARIABLE httVareSalg AS HANDLE      NO-UNDO.
  DEFINE VARIABLE httButiker  AS HANDLE      NO-UNDO.

  DEFINE VARIABLE hSourceTT  AS HANDLE      NO-UNDO.
  DEFINE VARIABLE lTranslate AS LOGICAL     NO-UNDO.
  DEFINE VARIABLE cServicehandel AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cHitrate AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cVgList AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE ii         AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cTmp AS CHARACTER   NO-UNDO.
  
  DEFINE TEMP-TABLE tt_aktsum NO-UNDO
    FIELD tid_txt  AS CHAR
    FIELD oms_verd AS DECI
    FIELD svk      AS DECI
    INDEX tid_txt IS PRIMARY UNIQUE tid_txt.

{onlinedatads.i}

DEFINE BUFFER bufTT_Butiker FOR TT_Butiker.
DEFINE BUFFER bufTT_SBudDW  FOR TT_SBudDW.
DEFINE BUFFER bufTT_SBudMY  FOR TT_SBudMY.

DEFINE VARIABLE buffer AS HANDLE NO-UNDO. 
DEFINE VARIABLE hRelation AS HANDLE EXTENT 3 NO-UNDO.
    
DEFINE VARIABLE bLogg AS LOGICAL NO-UNDO.
DEFINE VARIABLE cErrLoggFil AS CHARACTER NO-UNDO.

DEFINE BUFFER bufStLinje FOR StLinje.

ASSIGN 
    bLogg       = FALSE
    cErrLoggFil = 'asOnline'
    dDato       = IF dDato <> ? THEN dDato ELSE TODAY.

IF bLogg THEN 
  RUN bibl_loggDbFri.p (cErrLoggFil, 'onlinedatads.p: input parametre '
                        + ' Butikk: ' + STRING(iButikkNr)
                        + ' Direction: ' + cDirection
                        + ' Bruker: ' + cBrukerid
                        + ' Dato: ' + STRING(dDato)
                        ).

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
         HEIGHT             = 28.14
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */
/* cAvdFelter = "avdelingnr,AvdelingNavn,AntSolgt,salgbrutto,VerdiSolgt,VerdiRabatt,DBkr,DBproc,mvaverdi,vvarekost,GjenkjopAnt,GjenkjopVerdi". */
/* cAvdLabels = "Avdeling,Beskrivelse,Solgt,Salgssum brutto,Salgssum netto,Rabatter,DB kr,DB%,Mva,Varekost,Returer,Returer kr".                */
cAvdFelter = "avdelingnr,AvdelingNavn,AntSolgt,salgbrutto,VerdiSolgt,VerdiRabatt,DBkr,DBproc,vvarekost,GjenkjopAnt,GjenkjopVerdi".
cAvdLabels = "Avdeling,Beskrivelse,Solgt,Salgssum brutto,Salgssum netto,Rabatter,DB kr,DB%,Varekost,Returer,Returer kr".

RUN getRequestedDate. /* Här sätts datum */
RUN getWeekOffset. /* Ukedag offset */
RUN getButListe.
IF bLogg THEN 
  RUN bibl_loggDbFri.p (cErrLoggFil, 'onlinedatads.p: Butikkkiste '
                        + cButliste
                        ).    

{syspara.i 2 5 103 cServicehandel}
{syspara.i 210 275 1 cTmp}
/* Bara JF tillsvidare */
    IF cTmp <> "" THEN DO:
        cHitrate = "1".
        FOR EACH vargr WHERE vargr.hg = 5 NO-LOCK.
            IF NOT CAN-DO(cVgList,STRING(vargr.vg)) THEN
                cVgList = cVgList + (IF cVgList <> "" THEN "," ELSE "") + STRING(vargr.vg).
        END.
    END.
    /*  */
    OUTPUT TO c:\tmp\_aaa.txt APPEND.
    PUT UNFORMATTED "Kalle" SKIP.
    OUTPUT CLOSE.
    
IF lTranslate = TRUE THEN
    RUN oversett.
RUN fillTT.
/* RUN SkapaDynTT. */

CREATE DATASET hDset .
hDset:NAME = "OnlineReport". 
    
IF cBrukerId = 'Kasse' THEN DO:
    {syspara.i 6 10 2 cCustLayout}
    IF cCustLayout <> "" AND NUM-ENTRIES(cCustLayout) = 10 THEN
        cLayout = cCustLayout.
    ELSE
        ASSIGN cLayout = "0,0,1,0,1,1,1,1,1,1". 
END.
ELSE ASSIGN cLayout = "1,1,1,1,1,1,1,1,1,1".  

/*     hDset:ADD-BUFFER(httAvdeling:DEFAULT-BUFFER-HANDLE). */
  DO iCount = 1 TO NUM-ENTRIES(cLayout):
      CASE iCount:
          WHEN 1 THEN DO:
              IF ENTRY(iCount,cLayout) = "1" THEN /* Avdeling stat */
                  hDset:ADD-BUFFER(BUFFER TT_Avdeling:HANDLE).      
          END.
          WHEN 2 THEN DO:
              IF ENTRY(iCount,cLayout) = "1" THEN /* Hovedgr stat */
                  hDset:ADD-BUFFER(BUFFER TT_HuvGr:HANDLE).      
          END.
          WHEN 3 THEN DO:
              IF ENTRY(iCount,cLayout) = "1" THEN /* Varegruppe stat */
                  hDset:ADD-BUFFER(BUFFER TT_Vg:HANDLE).      
          END.
          WHEN 4 THEN DO:
              IF ENTRY(iCount,cLayout) = "1" THEN /* Levstat */
                  hDset:ADD-BUFFER(BUFFER TT_Lev:HANDLE).      
          END.
          WHEN 5 THEN DO:
              IF ENTRY(iCount,cLayout) = "1" THEN /* Selgerstat */
                  hDset:ADD-BUFFER(BUFFER TT_Selger:HANDLE).      
          END.
          WHEN 6 THEN DO:
              IF ENTRY(iCount,cLayout) = "1" THEN /* Timesalg */
                  hDset:ADD-BUFFER(BUFFER TT_akt_rapp:HANDLE).
          END.
          WHEN 7 THEN DO:
              IF ENTRY(iCount,cLayout) = "1" THEN /* Artstat */
                  hDset:ADD-BUFFER(BUFFER TT_VareSalg:HANDLE).      
          END.
          WHEN 8 THEN DO:
              IF ENTRY(iCount,cLayout) = "1" THEN /* Butikkstat */
                  hDset:ADD-BUFFER(BUFFER TT_Butiker:HANDLE).
          END.
          WHEN 9 THEN DO:
              IF ENTRY(iCount,cLayout) = "1" THEN /* Budsjett DW */
                  hDset:ADD-BUFFER(BUFFER TT_SBudDW:HANDLE).
          END.
          WHEN 10 THEN DO:
              IF ENTRY(iCount,cLayout) = "1" THEN /* Budsjett MY */
                  hDset:ADD-BUFFER(BUFFER TT_SBudMY:HANDLE).
          END.
      END CASE.
  END.


/* Skriver datasett til JSon fil. */
/* bOk = hDset:WRITE-JSON("file", 'hDset' + REPLACE(STRING(TODAY),'/','') + '.json', TRUE). */

/*
 TEMP-TABLE tt_butiker:WRITE-XML("file","c:\tmp\tt_butker.xml",true,?,?,true). 

 hDset:WRITE-XML("file","c:\tmp\Online.xml",true,?,?,true). 

 hTable = TEMP-TABLE TT_Data:HANDLE. */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-AktRappFlik) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AktRappFlik Procedure 
PROCEDURE AktRappFlik :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
/* vi har bara 1 butik därför ingen butik i tt */
/*   DEFINE TEMP-TABLE tt_aktsum NO-UNDO          */
/*       FIELD tid_txt AS CHAR                    */
/*       FIELD oms_verd AS DECI                   */
/*       FIELD DBKr     AS DECI                   */
/*       INDEX tid_txt IS PRIMARY UNIQUE tid_txt. */
  DO iCount = 1 TO 1 /* NUM-ENTRIES(cButListe) */:
      FOR EACH akt_rapp NO-LOCK WHERE akt_rapp.dato = dDato AND akt_rapp.butik = iButikknr /* INT(ENTRY(icount,cButliste)) */ AND
             (akt_rapp.oms_verd <> 0 OR akt_rapp.ant_kunder <> 0):
          FIND FIRST TT_akt_rapp WHERE TT_akt_rapp.tid_txt = akt_rapp.tid_txt NO-ERROR.
          IF AVAIL TT_akt_rapp THEN DO:
              FIND tt_aktsum WHERE tt_aktsum.tid_txt = akt_rapp.tid_txt. /* finns tt_akt-rapp så finns tt_aktsum */
              ASSIGN
/*               TT_akt_rapp.oms_verd   = TT_akt_rapp.oms_verd   + akt_rapp.oms_verd  */
/*                   TT_akt_rapp.svk        = TT_akt_rapp.svk        + akt_rapp.svk */
/*                   TT_akt_rapp.mva_kr     = TT_akt_rapp.mva_kr     + akt_rapp.mva_kr */
              TT_akt_rapp.bruttosalg = TT_akt_rapp.bruttosalg + akt_rapp.oms_verd + akt_rapp.mva_kr
              TT_akt_rapp.ant_kunder = TT_akt_rapp.ant_kunder + akt_rapp.ant_kunder
              TT_akt_rapp.ant_kvitto = TT_akt_rapp.ant_kvitto + akt_rapp.ant_kvitto
              TT_akt_rapp.verd_ret   = TT_akt_rapp.verd_ret   + akt_rapp.verd_ret
              TT_akt_rapp.ant_ret    = TT_akt_rapp.ant_ret    + akt_rapp.ant_ret.

              ASSIGN tt_aktsum.oms_verd = tt_aktsum.oms_verd + akt_rapp.oms_verd
                     tt_aktsum.svk      = tt_aktsum.svk      + akt_rapp.svk.
              /*  */

          END.
          ELSE DO:
              CREATE TT_akt_rapp.
              ASSIGN
                 TT_akt_rapp.butik      = akt_rapp.butik
                 TT_akt_rapp.tid_txt    = akt_rapp.tid_txt   
/*                  TT_akt_rapp.oms_verd   = akt_rapp.oms_verd */
/*                  TT_akt_rapp.svk        = akt_rapp.svk */
/*                  TT_akt_rapp.mva_kr     = akt_rapp.mva_kr */
                 TT_akt_rapp.bruttosalg = akt_rapp.oms_verd + akt_rapp.mva_kr
                 TT_akt_rapp.ant_kunder = akt_rapp.ant_kunder
                 TT_akt_rapp.ant_kvitto = akt_rapp.ant_kvitto * -1
                 TT_akt_rapp.verd_ret   = akt_rapp.verd_ret  
                 TT_akt_rapp.ant_ret    = akt_rapp.ant_ret * -1.
/*               dDBKrtmp                = akt_rapp.oms_verd - akt_rapp.svk                                                     */
/*   /*              TT_akt_rapp.DBKr       = TT_akt_rapp.oms_verd - TT_akt_rapp.svk */                                         */
/*                TT_akt_rapp.DBproc        = IF akt_rapp.oms_verd = 0 THEN 0 ELSE ROUND(dDBKrtmp / akt_rapp.oms_verd * 100,2). */

              CREATE tt_aktsum.
              ASSIGN tt_aktsum.tid_txt   = akt_rapp.tid_txt
                     tt_aktsum.oms_verd  = akt_rapp.oms_verd
                     tt_aktsum.svk       = akt_rapp.svk.
          END.
      END.
      FOR EACH TT_akt_rapp:
          FIND tt_aktsum WHERE tt_aktsum.tid_txt = tt_akt_rapp.tid_txt.
          ASSIGN TT_akt_rapp.DBproc              = IF TT_aktsum.oms_verd = 0 THEN 0 ELSE ROUND((TT_aktsum.oms_verd - tt_aktsum.svk) / TT_aktsum.oms_verd * 100,2).
      END.
      EMPTY TEMP-TABLE tt_aktsum.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AktRappFlikOrg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AktRappFlikOrg Procedure 
PROCEDURE AktRappFlikOrg :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DO iCount = 1 TO 1 /* NUM-ENTRIES(cButListe) */:
      FOR EACH akt_rapp NO-LOCK WHERE akt_rapp.dato = dDato AND akt_rapp.butik = iButikknr /* INT(ENTRY(icount,cButliste)) */ AND
             (akt_rapp.oms_verd <> 0 OR akt_rapp.ant_kunder <> 0):
          FIND FIRST TT_akt_rapp WHERE TT_akt_rapp.tid_txt = akt_rapp.tid_txt NO-ERROR.
          IF AVAIL TT_akt_rapp THEN DO:
              ASSIGN
/*               TT_akt_rapp.oms_verd   = TT_akt_rapp.oms_verd   + akt_rapp.oms_verd  */
/*                   TT_akt_rapp.svk        = TT_akt_rapp.svk        + akt_rapp.svk */
/*                   TT_akt_rapp.mva_kr     = TT_akt_rapp.mva_kr     + akt_rapp.mva_kr */
              TT_akt_rapp.bruttosalg = TT_akt_rapp.bruttosalg + akt_rapp.oms_verd + akt_rapp.mva_kr
              TT_akt_rapp.ant_kunder = TT_akt_rapp.ant_kunder + akt_rapp.ant_kunder
              TT_akt_rapp.ant_kvitto = TT_akt_rapp.ant_kvitto + akt_rapp.ant_kvitto
              TT_akt_rapp.verd_ret   = TT_akt_rapp.verd_ret   + akt_rapp.verd_ret
              TT_akt_rapp.ant_ret    = TT_akt_rapp.ant_ret    + akt_rapp.ant_ret.
          END.
          ELSE DO:
              CREATE TT_akt_rapp.
              ASSIGN
                 TT_akt_rapp.butik      = akt_rapp.butik
                 TT_akt_rapp.tid_txt    = akt_rapp.tid_txt   
/*                  TT_akt_rapp.oms_verd   = akt_rapp.oms_verd */
/*                  TT_akt_rapp.svk        = akt_rapp.svk */
/*                  TT_akt_rapp.mva_kr     = akt_rapp.mva_kr */
                 TT_akt_rapp.bruttosalg = akt_rapp.oms_verd + akt_rapp.mva_kr
                 TT_akt_rapp.ant_kunder = akt_rapp.ant_kunder
                 TT_akt_rapp.ant_kvitto = akt_rapp.ant_kvitto * -1
                 TT_akt_rapp.verd_ret   = akt_rapp.verd_ret  
                 TT_akt_rapp.ant_ret    = akt_rapp.ant_ret * -1
              dDBKrtmp                = akt_rapp.oms_verd - akt_rapp.svk
  /*              TT_akt_rapp.DBKr       = TT_akt_rapp.oms_verd - TT_akt_rapp.svk */
               TT_akt_rapp.DBproc        = IF akt_rapp.oms_verd = 0 THEN 0 ELSE ROUND(dDBKrtmp / akt_rapp.oms_verd * 100,2).


          END.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ArtikkelFlik) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ArtikkelFlik Procedure 
PROCEDURE ArtikkelFlik :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

  DO iCount = 1 TO 1 /* NUM-ENTRIES(cButListe) */:
      FOR EACH StLinje WHERE StLinje.Butik = iButikknr /* INT(ENTRY(icount,cButliste)) */ AND StTypeId = 'ARTIKKEL' AND
       PerId = 'DAG' AND AarPerLinNr = iAarPerLinNr AND (StLinje.AntSolgt <> 0 OR (StLinje.AntSolgt = 0 AND StLinje.GjenkjopAnt <> 0)) USE-INDEX AarPerLinNr NO-LOCK:
          FIND TT_VareSalg WHERE TT_VareSalg.ArtikkelNr    = DECI(StLinje.DataObjekt) NO-ERROR.
          IF NOT AVAIL TT_VareSalg THEN DO:
              RELEASE Farg.
              RELEASE Varemerke.
              FIND ArtBas WHERE ArtBas.ArtikkelNr = DECI(StLinje.DataObjekt) NO-LOCK NO-ERROR.
              IF AVAIL ArtBas THEN DO:
                  FIND Farg OF ArtBas NO-LOCK NO-ERROR.
/*                   FIND varemerke OF artbas NO-LOCK NO-ERROR. */
              END.
              CREATE TT_VareSalg.
              ASSIGN
                TT_VareSalg.ArtikkelNr    = DECI(StLinje.DataObjekt)
                TT_VareSalg.LevKod        = IF AVAIL ArtBas THEN ArtBas.LevKod ELSE " "
                TT_VareSalg.Farg          = IF AVAIL Farg THEN Farg.FarBeskr ELSE ArtBas.LevFargkod
                TT_VareSalg.Beskr         = IF AVAIL ArtBas THEN SUBSTR(ArtBas.Beskr,1,25) ELSE "?? " + SUBSTR(StLinje.Beskrivelse,1,25).
/*                 TT_VareSalg.Varemerke    = IF AVAIL Varemerke THEN Varemerke.Beskrivelse ELSE "". */
          END.
          ASSIGN
            TT_VareSalg.salgbrutto    = TT_VareSalg.salgbrutto  + StLinje.VerdiSolgt + StLinje.MvaVerdi
/*             TT_VareSalg.VerdiSolgt    = TT_VareSalg.VerdiSolgt  + StLinje.VerdiSolgt */
            TT_VareSalg.AntSolgt      = TT_VareSalg.AntSolgt    + StLinje.AntSolgt
            TT_VareSalg.VerdiRabatt   = TT_VareSalg.VerdiRabatt + StLinje.VerdiRabatt
/*             TT_VareSalg.DBkr          = TT_VareSalg.DBkr        + StLinje.VerdiSolgt - StLinje.VVarekost */
              dDBKrtmp                = StLinje.VerdiSolgt - StLinje.VVarekost
              TT_VareSalg.DBproc           = IF StLinje.VerdiSolgt <= 0 THEN 0 ELSE ROUND(dDBKrtmp / StLinje.VerdiSolgt * 100,2)
/*             TT_VareSalg.mvaverdi      = TT_VareSalg.mvaverdi      + StLinje.MvaVerdi */
/*             TT_VareSalg.vvarekost     = TT_VareSalg.vvarekost     + StLinje.VVarekost */
            TT_VareSalg.GjenkjopAnt   = TT_VareSalg.GjenkjopAnt   + StLinje.GjenkjopAnt
            TT_VareSalg.GjenkjopVerdi = TT_VareSalg.GjenkjopVerdi + StLinje.GjenkjopVerdi.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-AvdelingFlik) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AvdelingFlik Procedure 
PROCEDURE AvdelingFlik :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

  DO iCount = 1 TO 1 /* NUM-ENTRIES(cButListe) */:
      FOR EACH StLinje WHERE StLinje.Butik = iButikknr /* INT(ENTRY(icount,cButliste)) */ AND StTypeId = 'AVDELING' AND
          PerId = 'DAG' AND AarPerLinNr = iAarPerLinNr /* AND StLinje.VerdiSolgt <> 0 */ USE-INDEX AarPerLinNr NO-LOCK:
          FIND TT_Avdeling WHERE TT_Avdeling.avdelingnr    = INT(StLinje.DataObjekt) NO-ERROR.
          IF NOT AVAIL TT_Avdeling THEN DO:
              FIND Avdeling WHERE Avdeling.AvdelingNr = INT(StLinje.DataObjekt) NO-LOCK NO-ERROR.
              CREATE TT_Avdeling.
              ASSIGN
                TT_Avdeling.avdelingnr    = INT(StLinje.DataObjekt)
                TT_Avdeling.AvdelingNavn  = IF AVAIL Avdeling THEN Avdeling.AvdelingNavn ELSE "?? " + StLinje.Beskrivelse.
          END.
          ASSIGN
            TT_Avdeling.salgbrutto    = TT_Avdeling.salgbrutto  + StLinje.VerdiSolgt + StLinje.MvaVerdi
/*             TT_Avdeling.VerdiSolgt    = TT_Avdeling.VerdiSolgt  + StLinje.VerdiSolgt */
            TT_Avdeling.AntSolgt      = TT_Avdeling.AntSolgt    + StLinje.AntSolgt
            TT_Avdeling.VerdiRabatt   = TT_Avdeling.VerdiRabatt + StLinje.VerdiRabatt
/*             TT_Avdeling.DBkr          = TT_Avdeling.DBkr        + StLinje.VerdiSolgt - StLinje.VVarekost */
            dDBKrtmp                  = StLinje.VerdiSolgt - StLinje.VVarekost
            TT_Avdeling.DBproc        = IF StLinje.VerdiSolgt <= 0 THEN 0 ELSE ROUND(dDBKrtmp / StLinje.VerdiSolgt * 100,2)
/*             TT_Avdeling.mvaverdi      = TT_Avdeling.mvaverdi      + StLinje.MvaVerdi */
/*             TT_Avdeling.vvarekost     = TT_Avdeling.vvarekost     + StLinje.VVarekost */
            TT_Avdeling.GjenkjopAnt   = TT_Avdeling.GjenkjopAnt   + StLinje.GjenkjopAnt
            TT_Avdeling.GjenkjopVerdi = TT_Avdeling.GjenkjopVerdi + StLinje.GjenkjopVerdi.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ButikkFlik) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ButikkFlik Procedure 
PROCEDURE ButikkFlik :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

  IF iSumbutNr > 0 THEN 
  DO:
      FIND bufTT_Butiker WHERE 
          buftt_Butiker.butik = iSumbutNr NO-ERROR.
      IF NOT AVAILABLE bufTT_butiker THEN 
          DO:
              CREATE bufTT_Butiker.
              ASSIGN 
                  buftt_Butiker.Butik   = iSumbutNr
                  buftt_Butiker.ButNamn = 'Sum'
                  .
          END.    
  END.
  ASSIGN   
    dSumDBKrtmp    = 0  
    dSumVerdiSolgt = 0
    .

/*   FOR EACH Butiker NO-LOCK: */
  BUTIKK-FLIK:
  DO iCount = 1 TO NUM-ENTRIES(cButListe):
      /* syspara: om 1 så får man se butiker som inte tillhör brgrp */
/*       IF cSeAllaiButStat <> "1" AND NOT CAN-DO(cButListe,STRING(Butiker.butik)) THEN */
/*           NEXT.                                                                      */
      FIND Butiker WHERE Butiker.butik = INT(ENTRY(icount,cButliste)) NO-LOCK NO-ERROR.
      IF bLogg THEN 
        RUN bibl_loggDbFri.p (cErrLoggFil, 'onlinedatads.p: FillTT '
                              + "Butikk: " + (IF AVAILABLE Butiker THEN STRING(Butiker.Butik) ELSE 'NOT AVAIL')
                              ).    
      IF NOT AVAIL Butiker THEN
          NEXT.
          
      FIND FIRST SBudHode NO-LOCK WHERE 
          SBudHode.ButikkNr = Butiker.Butik AND 
          SBudHode.Aar      = YEAR(dDato) AND 
          SBudHode.Aktiv    = TRUE NO-ERROR.
      IF bLogg THEN 
          RUN bibl_loggDbFri.p (cErrLoggFil, 'onlinedatads.p: FillTT ButStat '
                                + ' Butikk: ' + (IF AVAILABLE SBudHode THEN STRING(SBudHode.ButikkNr) ELSE ' ')
                                + ' SBudHode Aar: ' + (IF AVAILABLE SBudHode THEN STRING(SBudHode.Aar) ELSE ' ')
                                + ' SBudHode Aktiv: ' + (IF AVAILABLE SBudHode THEN STRING(SBudHode.Aktiv) ELSE 'Ikke tilgjengelig')
                                + ' SBudHode Avail: ' + STRING(AVAILABLE SBudHode)
                                ).    
          
      ASSIGN 
        iAntkunder     = 0
        iAntkvitt      = 0
        . 
      FOR EACH StLinje WHERE 
          StLinje.Butik = Butiker.Butik AND 
          StTypeId      = 'BUTSTAT' AND
          PerId         = 'DAG' AND 
          AarPerLinNr   = iAarPerLinNr 
          /* AND StLinje.AntSolgt <> 0 */ 
          USE-INDEX AarPerLinNr NO-LOCK:

          FOR EACH akt_rapp NO-LOCK WHERE akt_rapp.dato = dDato AND akt_rapp.butik = StLinje.Butik AND
                 (akt_rapp.oms_verd <> 0 OR akt_rapp.ant_kunder <> 0):
              ASSIGN iAntkunder = iAntkunder + akt_rapp.ant_kunder
                     iAntkvitt  = iAntkvitt + (akt_rapp.ant_kvitto * -1).
          END.
         
         IF AVAILABLE SBudDag THEN RELEASE SBudDag.
         IF AVAILABLE SBudHode THEN 
             FIND SBudDag NO-LOCK WHERE 
                 SBudDag.SBudId    = SBudHode.SBudId AND 
                 SBudDag.AarMnd    = INT(STRING(YEAR(dDato),"9999") + STRING(MONTH(dDato),"99")) AND 
                 SBudDag.AarMndDag = INT(STRING(YEAR(dDato),"9999") + STRING(MONTH(dDato),"99") + STRING(DAY(dDato),"99"))  NO-ERROR.
  
          CREATE TT_Butiker.
          ASSIGN
            TT_Butiker.butik         = Butiker.butik
            TT_Butiker.butnamn       = Butiker.butnamn
            TT_Butiker.salgbrutto    = StLinje.VerdiSolgt + StLinje.MvaVerdi
            TT_Butiker.SalgBudsjett  = IF AVAILABLE SBudDag THEN SBudDag.SalgBudsjett ELSE 0
/*             TT_Butiker.VerdiSolgt    = StLinje.VerdiSolgt */
            TT_Butiker.AntSolgt      = StLinje.AntSolgt
            TT_Butiker.VerdiRabatt   = StLinje.VerdiRabatt
/*             TT_Butiker.DBkr          = TT_Butiker.VerdiSolgt - StLinje.VVarekost */
            dDBKrtmp                 =  StLinje.VerdiSolgt - StLinje.VVarekost
            dSumDBKrtmp              = dSumDBKrtmp + dDBKrtmp 
            dSumVerdiSolgt           = dSumVerdiSolgt + StLinje.VerdiSolgt 
            TT_Butiker.DBproc           = IF StLinje.VerdiSolgt = 0 THEN 0 ELSE ROUND(dDBKrtmp / StLinje.VerdiSolgt * 100,2)
/*             TT_Butiker.mvaverdi      = StLinje.MvaVerdi */
/*             TT_Butiker.vvarekost     = StLinje.VVarekost */
            TT_Butiker.GjenkjopAnt   = StLinje.GjenkjopAnt
            TT_Butiker.GjenkjopVerdi = StLinje.GjenkjopVerdi
            TT_Butiker.Antkunder     = iAntkunder
            TT_Butiker.Antkvitt      = iAntkvitt
            .

          IF AVAILABLE bufTT_Butiker THEN 
          DO:
              ASSIGN 
                bufTT_Butiker.salgbrutto    = bufTT_Butiker.salgbrutto     + TT_Butiker.salgbrutto   
                bufTT_Butiker.SalgBudsjett  = bufTT_Butiker.SalgBudsjett   + TT_Butiker.SalgBudsjett 
                bufTT_Butiker.AntSolgt      = bufTT_Butiker.AntSolgt       + TT_Butiker.AntSolgt     
                bufTT_Butiker.VerdiRabatt   = bufTT_Butiker.VerdiRabatt    + TT_Butiker.VerdiRabatt  
                bufTT_Butiker.GjenkjopAnt   = bufTT_Butiker.GjenkjopAnt    + TT_Butiker.GjenkjopAnt  
                bufTT_Butiker.GjenkjopVerdi = bufTT_Butiker.GjenkjopVerdi  + TT_Butiker.GjenkjopVerdi
                bufTT_Butiker.Antkunder     = bufTT_Butiker.Antkunder      + TT_Butiker.Antkunder    
                bufTT_Butiker.Antkvitt      = bufTT_Butiker.Antkvitt       + TT_Butiker.Antkvitt     
              .              
/* MESSAGE 'Akkumulerer bufTT_Butiker tilgjengelig ' AVAILABLE bufTT_Butiker. */
          END.
      END.
  END. /* BUTIKK-FLIK */
  IF AVAILABLE bufTT_Butiker THEN 
  DO:
    bufTT_Butiker.DBproc = IF dSumVerdiSolgt = 0 THEN 0 ELSE ROUND(dSumDBKrtmp / dSumVerdiSolgt * 100,2).
/* MESSAGE 'db% tilgjengelig ' AVAILABLE bufTT_Butiker. */
    
  END.
  

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fillTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fillTT Procedure 
PROCEDURE fillTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    EMPTY TEMP-TABLE TT_Vg.
    EMPTY TEMP-TABLE TT_Lev.
    EMPTY TEMP-TABLE TT_Selger.
    EMPTY TEMP-TABLE TT_HuvGr.
    EMPTY TEMP-TABLE TT_Avdeling.
    EMPTY TEMP-TABLE TT_VareSalg.
    EMPTY TEMP-TABLE TT_akt_rapp.
    EMPTY TEMP-TABLE TT_butiker.
    EMPTY TEMP-TABLE TT_SBudDW.
    EMPTY TEMP-TABLE TT_SBudMY.
      
    /*   cButliste = cTillgbutiker. */
    
    ASSIGN
        dNetRab    = 0
        iCount     = 0
        iAntKunder = 0
        iAntKvitt  = 0
        dDBKrtmp   = 0.
    
    IF bLogg THEN 
      RUN bibl_loggDbFri.p (cErrLoggFil, 'onlinedatads.p: FillTT '
                            + "Butlst: " + cButliste
                            ).    
    
    RUN ButikkFlik.
    RUN LeverandorFlik.
    RUN SelgerFlik.
    RUN VaregruppeFlik.  
    RUN HovedgruppeFlik.
    RUN AvdelingFlik.
    RUN ArtikkelFlik.
    RUN AktRappFlik.
    RUN SalgsbudsjettDWFlik.
    RUN SalgsbudsjettMYFlik.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getButListe) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getButListe Procedure 
PROCEDURE getButListe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTillgButiker AS CHARACTER   NO-UNDO.
    
    /* Sjekker om det er overstyring av hvilke butikker som skal vises i budsjettet */
    /* Er ikke parametre satt, styres tilgang av brukergruppe og rapport team.      */
    /* For å få sum butikk, må det legges inn overstyring.                          */
    SJEKK_SYSPARA:
    DO:
        {syspara.i 6 10 3 cTekst}
        IF CAN-DO(cTekst,STRING(ibutikkNr)) THEN 
            cButliste = cTekst.
        IF cButliste = '' THEN 
        DO:    
            {syspar2.i 6 10 3 cTekst}
            IF CAN-DO(cTekst,STRING(ibutikkNr)) THEN 
                cButliste = cTekst.
            /* Setter sumbutikknr. hvis dette er angitt i systemparameter. */
            {syspar2.i 6 10 5 iSumButNr INT}
            IF NOT CAN-FIND(Butiker WHERE 
                            Butiker.Butik = iSumbutNr) THEN 
                iSumButNr = 0.                                
        END. 
        ELSE DO:
            {syspara.i 6 10 5 iSumButNr INT}
        END.
    END. /* SJEKK_SYSPARA */
    
    IF cButliste = '' THEN 
    DO:
        cButliste = STRING(iButikkNr).
        
        FIND bruker WHERE bruker.brukerid = cBrukerid NO-LOCK NO-ERROR.
        IF bruker.lng = "SE" OR bruker.lng = "SVE" THEN
            lTranslate = TRUE.
        IF AVAIL bruker THEN DO:
            FOR EACH ButikkTeam NO-LOCK WHERE ButikkTeam.BrGrpNr = Bruker.BrGrpNr AND
                                      ButikkTeam.TeamTypeId = 2 BY ButikkTeam.Beskrivelse.
                FOR EACH ButikkKobling OF ButikkTeam.
                    IF ButikkKobling.butik = iButikkNr THEN
                        NEXT.
                    IF CAN-DO(cTillgButiker,STRING(ButikkKobling.butik)) THEN
                        NEXT.
                    FIND butiker WHERE butiker.butik = ButikkKobling.butik NO-LOCK NO-ERROR.
                    IF NOT AVAIL butiker THEN
                        NEXT.
                    IF butiker.nedlagtdato <> ? AND butiker.nedlagtdato < dDato THEN
                        NEXT.
                    IF Butiker.harButikksystem = FALSE THEN
                        NEXT.
                    cTillgbutiker = cTillgbutiker + (IF cTillgbutiker <> "" THEN "," ELSE "") + STRING(butiker.butik).
                END.
            END.
            IF cTillgbutiker <> "" THEN
                cButliste = cButliste + "," + cTillgbutiker.
        END.
    END.
/*     MESSAGE 'cbutLst: ' cButListe.   */
/*     MESSAGE 'Sumbutikk: ' iSumButNr. */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getRequestedDate) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getRequestedDate Procedure 
PROCEDURE getRequestedDate :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF cDirection = "C" THEN
        dDato = TODAY.
    ASSIGN iAarPerLinNr = YEAR(dDato) * 1000 + dDato - DATE(12,31,YEAR(dDato) - 1).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-getWeekOffset) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getWeekOffset Procedure 
PROCEDURE getWeekOffset :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:    
                                  1,2,3,4,5,6,7 
                        wdString  7,1,2,3,4,5,6                                                                                                   
        ------------------------------------------------------------------------------*/

  ASSIGN
    cDayString = '7,1,2,3,4,5,6'
    iWdOff     = WEEKDAY(dDato)
    iWdOff     = INT(ENTRY(iWdOff,cDayString)) - 1
    . 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-HovedgruppeFlik) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HovedgruppeFlik Procedure 
PROCEDURE HovedgruppeFlik :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

  DO iCount = 1 TO 1 /* NUM-ENTRIES(cButListe) */:
      FOR EACH StLinje WHERE StLinje.Butik = iButikknr /* INT(ENTRY(icount,cButliste)) */ AND StTypeId = 'HOVEDGR' AND
          PerId = 'DAG' AND AarPerLinNr = iAarPerLinNr /* AND StLinje.VerdiSolgt <> 0 */ USE-INDEX AarPerLinNr NO-LOCK:
          FIND TT_HuvGr WHERE TT_HuvGr.hg = INT(StLinje.DataObjekt) NO-ERROR.
          IF NOT AVAIL TT_HuvGr THEN DO:
              FIND HuvGr WHERE HuvGr.Hg = INT(StLinje.DataObjekt) NO-LOCK NO-ERROR.
              CREATE TT_HuvGr.
              ASSIGN
                TT_HuvGr.hg            = INT(StLinje.DataObjekt)
                TT_HuvGr.HgBeskr       = IF AVAIL HuvGr THEN HuvGr.hgbeskr ELSE "?? " + StLinje.Beskrivelse.
          END.
          ASSIGN
            TT_HuvGr.salgbrutto    = TT_HuvGr.salgbrutto  + StLinje.VerdiSolgt + StLinje.MvaVerdi
/*             TT_HuvGr.VerdiSolgt    = TT_HuvGr.VerdiSolgt  + StLinje.VerdiSolgt */
            TT_HuvGr.AntSolgt      = TT_HuvGr.AntSolgt    + StLinje.AntSolgt
            TT_HuvGr.VerdiRabatt   = TT_HuvGr.VerdiRabatt + StLinje.VerdiRabatt
            dDBKrtmp               =  StLinje.VerdiSolgt - StLinje.VVarekost
            TT_HuvGr.DBproc           = IF StLinje.VerdiSolgt <= 0 THEN 0 ELSE ROUND(dDBKrtmp / StLinje.VerdiSolgt * 100,2)
/*             TT_HuvGr.mvaverdi      = TT_HuvGr.mvaverdi      + StLinje.MvaVerdi */
/*             TT_HuvGr.vvarekost     = TT_HuvGr.vvarekost     + StLinje.VVarekost */
            TT_HuvGr.GjenkjopAnt   = TT_HuvGr.GjenkjopAnt   + StLinje.GjenkjopAnt
            TT_HuvGr.GjenkjopVerdi = TT_HuvGr.GjenkjopVerdi + StLinje.GjenkjopVerdi.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-LeverandorFlik) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LeverandorFlik Procedure 
PROCEDURE LeverandorFlik :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

  LEVERANDO-FLIK:
  DO iCount = 1 TO 1 /* NUM-ENTRIES(cButListe) */:
      FOR EACH StLinje WHERE StLinje.Butik = iButikknr /* INT(ENTRY(icount,cButliste)) */ AND StTypeId = 'LEVERAN' AND
          PerId = 'DAG' AND AarPerLinNr = iAarPerLinNr /* AND StLinje.VerdiSolgt <> 0 */ USE-INDEX AarPerLinNr NO-LOCK:
          FIND TT_Lev WHERE TT_Lev.levnr = INT(StLinje.DataObjekt) NO-ERROR.
          IF NOT AVAIL TT_Lev THEN DO:
              FIND LevBas WHERE LevBas.levnr = INT(StLinje.DataObjekt) NO-LOCK NO-ERROR.
              CREATE TT_Lev.
              ASSIGN
                TT_Lev.levnr         = INT(StLinje.DataObjekt)
                TT_Lev.levnavn       = IF AVAIL Levbas THEN levbas.levnamn ELSE "?? " + StLinje.Beskrivelse.
          END.
          ASSIGN
            TT_Lev.salgbrutto  = TT_Lev.salgbrutto    + StLinje.VerdiSolgt + StLinje.MvaVerdi
/*             TT_Lev.VerdiSolgt  = TT_Lev.VerdiSolgt    + StLinje.VerdiSolgt */
            TT_Lev.AntSolgt    = TT_Lev.AntSolgt      + StLinje.AntSolgt
            TT_Lev.VerdiRabatt = TT_Lev.VerdiRabatt   + StLinje.VerdiRabatt
/*             TT_Lev.DBkr        = TT_Lev.DBkr          + StLinje.VerdiSolgt - StLinje.VVarekost */
            dDBKrtmp           = StLinje.VerdiSolgt - StLinje.VVarekost
            TT_Lev.DBproc      = IF StLinje.VerdiSolgt <= 0 THEN 0 ELSE ROUND(dDBKrtmp / StLinje.VerdiSolgt * 100,2)
/*             TT_Lev.mvaverdi      = TT_Lev.mvaverdi      + StLinje.MvaVerdi */
/*             TT_Lev.vvarekost     = TT_Lev.vvarekost     + StLinje.VVarekost */
            TT_Lev.GjenkjopAnt   = TT_Lev.GjenkjopAnt   + StLinje.GjenkjopAnt
            TT_Lev.GjenkjopVerdi = TT_Lev.GjenkjopVerdi + StLinje.GjenkjopVerdi.
      END.
  END. /* LEVERANDOR-FLIK */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-oversett) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE oversett Procedure 
PROCEDURE oversett :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/* DEFINE TEMP-TABLE TT_akt_rapp NO-UNDO  SERIALIZE-NAME "Timesalg" */
    BUFFER TT_akt_rapp:HANDLE:BUFFER-FIELD("butik"):LABEL = "Butik".  
    BUFFER TT_akt_rapp:HANDLE:BUFFER-FIELD("tid_txt"):LABEL = "Kl".  
    BUFFER TT_akt_rapp:HANDLE:BUFFER-FIELD("bruttosalg"):LABEL = "Sålt brutto".
    BUFFER TT_akt_rapp:HANDLE:BUFFER-FIELD("DBproc"):LABEL = "TB%".
/*     FIELD ant_kunder     AS inte LABEL "Kunder" */
    BUFFER TT_akt_rapp:HANDLE:BUFFER-FIELD("ant_kvitto"):LABEL = "Kvitton".
/*     FIELD ant_ret        AS inte LABEL "Returer"    */
/*     FIELD verd_ret       AS deci LABEL "Returer kr" */

/* DEFINE TEMP-TABLE TT_Butiker NO-UNDO   SERIALIZE-NAME "Butikker" */
    BUFFER TT_Butiker:HANDLE:BUFFER-FIELD("butik"):LABEL = "Butik".  
    BUFFER TT_Butiker:HANDLE:BUFFER-FIELD("butnamn"):LABEL = "Namn".
    BUFFER TT_Butiker:HANDLE:BUFFER-FIELD("AntSolgt"):LABEL = "Sålt".
    BUFFER TT_Butiker:HANDLE:BUFFER-FIELD("salgbrutto"):LABEL = "Sålt brutto".
    BUFFER TT_Butiker:HANDLE:BUFFER-FIELD("salgBudsjett"):LABEL = IF cServicehandel <> "1" THEN "Budget kr" ELSE "Fg. år".
    BUFFER TT_Butiker:HANDLE:BUFFER-FIELD("DBproc"):LABEL = "TB%".
/*     FIELD VerdiRabatt   AS  DECI LABEL "Rabatter netto" */
/*     FIELD GjenkjopAnt   AS  DECI LABEL "Returer"        */
/*     FIELD GjenkjopVerdi AS  DECI LABEL "Returer kr"     */
/*     FIELD Antkunder     AS  INTE LABEL  "Kunder"        */
    BUFFER TT_Butiker:HANDLE:BUFFER-FIELD("Antkvitt"):LABEL = "Kvitton".

/* DEFINE TEMP-TABLE TT_Avdeling NO-UNDO SERIALIZE-NAME "Avdeling" */
    BUFFER TT_Avdeling:HANDLE:BUFFER-FIELD("avdelingnr"):LABEL = "Avdelning".
    BUFFER TT_Avdeling:HANDLE:BUFFER-FIELD("AvdelingNavn"):LABEL = "Namn".
    BUFFER TT_Avdeling:HANDLE:BUFFER-FIELD("AntSolgt"):LABEL = "Sålt".
    BUFFER TT_Avdeling:HANDLE:BUFFER-FIELD("salgbrutto"):LABEL = "Sålt brutto".
    BUFFER TT_Avdeling:HANDLE:BUFFER-FIELD("DBproc"):LABEL = "TB%".
/*     FIELD VerdiRabatt   AS  DECI LABEL "Rabatter netto" */
/*     FIELD GjenkjopAnt   AS  DECI LABEL "Returer"        */
/*     FIELD GjenkjopVerdi AS  DECI LABEL "Returer kr"     */

/* DEFINE TEMP-TABLE TT_Lev NO-UNDO SERIALIZE-NAME "Leverandør" */
  BUFFER TT_Lev:HANDLE:BUFFER-FIELD("levnr"):LABEL = "Levnr".
  BUFFER TT_Lev:HANDLE:BUFFER-FIELD("LevNavn"):LABEL = "Namn".
  BUFFER TT_Lev:HANDLE:BUFFER-FIELD("AntSolgt"):LABEL = "Sålt".
  BUFFER TT_Lev:HANDLE:BUFFER-FIELD("salgbrutto"):LABEL = "Sålt brutto".
  BUFFER TT_Lev:HANDLE:BUFFER-FIELD("DBproc"):LABEL = "TB%".
/*   BUFFER TT_Lev:HANDLE:BUFFER-FIELD("VerdiRabatt   AS  DECI LABEL "Rabatter netto" */
/*   BUFFER TT_Lev:HANDLE:BUFFER-FIELD("GjenkjopAnt   AS  DECI LABEL "Returer"        */
/*   BUFFER TT_Lev:HANDLE:BUFFER-FIELD("GjenkjopVerdi AS  DECI LABEL "Returer kr"     */

/* DEFINE TEMP-TABLE TT_Selger NO-UNDO  SERIALIZE-NAME "Selgere" */
  BUFFER TT_Selger:HANDLE:BUFFER-FIELD("selgernr"):LABEL = "Säljare".
  BUFFER TT_Selger:HANDLE:BUFFER-FIELD("Navn"):LABEL = "Namn".
  BUFFER TT_Selger:HANDLE:BUFFER-FIELD("AntSolgt"):LABEL = "Sålt".
  BUFFER TT_Selger:HANDLE:BUFFER-FIELD("salgbrutto"):LABEL = "Sålt brutto".
  BUFFER TT_Selger:HANDLE:BUFFER-FIELD("DBproc"):LABEL = "TB%".
/*   BUFFER TT_Selger:HANDLE:BUFFER-FIELD("VerdiRabatt   AS  DECI LABEL "Rabatter netto" */
/*   BUFFER TT_Selger:HANDLE:BUFFER-FIELD("GjenkjopAnt   AS  DECI LABEL "Returer"        */
/*   BUFFER TT_Selger:HANDLE:BUFFER-FIELD("GjenkjopVerdi AS  DECI LABEL "Returer kr"     */
/*   BUFFER TT_Selger:HANDLE:BUFFER-FIELD("AntKunder     AS  INTE LABEL "Kunder"         */


/* DEFINE TEMP-TABLE TT_HuvGr NO-UNDO  SERIALIZE-NAME "Hovedgrupper" */
/*   BUFFER TT_HuvGr:HANDLE:BUFFER-FIELD("hg            AS  INTE LABEL "Hg" */
  BUFFER TT_HuvGr:HANDLE:BUFFER-FIELD("HgBeskr"):LABEL = "Namn".
  BUFFER TT_HuvGr:HANDLE:BUFFER-FIELD("AntSolgt"):LABEL = "Sålt".
  BUFFER TT_HuvGr:HANDLE:BUFFER-FIELD("salgbrutto"):LABEL = "Sålt brutto".
  BUFFER TT_HuvGr:HANDLE:BUFFER-FIELD("DBproc"):LABEL = "TB%".
/*   FIELD VerdiRabatt   AS  DECI LABEL "Rabatter netto" */
/*   FIELD GjenkjopAnt   AS  DECI LABEL "Returer"        */
/*   FIELD GjenkjopVerdi AS  DECI LABEL "Returer kr"     */

/* DEFINE TEMP-TABLE TT_Vg NO-UNDO SERIALIZE-NAME "Varegrupper" */
/*   BUFFER TT_Vg:HANDLE:BUFFER-FIELD("vg            AS  INTE LABEL "Vg" */
  BUFFER TT_Vg:HANDLE:BUFFER-FIELD("VgBeskr"):LABEL = "Namn".
  BUFFER TT_Vg:HANDLE:BUFFER-FIELD("AntSolgt"):LABEL = "Sålt".
  BUFFER TT_Vg:HANDLE:BUFFER-FIELD("salgbrutto"):LABEL = "Sålt brutto".
  BUFFER TT_Vg:HANDLE:BUFFER-FIELD("DBproc"):LABEL = "TB%".
/*   FIELD VerdiRabatt   AS  DECI LABEL "Rabatter netto" */
/*   FIELD GjenkjopAnt   AS  DECI LABEL "Returer"        */
/*   FIELD GjenkjopVerdi AS  DECI LABEL "Returer kr"     */

/* DEFINE TEMP-TABLE TT_VareSalg NO-UNDO  SERIALIZE-NAME "Varesalg" */
    BUFFER TT_VareSalg:HANDLE:BUFFER-FIELD("Artikkelnr"):LABEL = "Artikelnr".
    BUFFER TT_VareSalg:HANDLE:BUFFER-FIELD("Beskr"):LABEL = "Namn".
    BUFFER TT_VareSalg:HANDLE:BUFFER-FIELD("Farg"):LABEL = "Färg".
    BUFFER TT_VareSalg:HANDLE:BUFFER-FIELD("AntSolgt"):LABEL = "Sålt".
    BUFFER TT_VareSalg:HANDLE:BUFFER-FIELD("salgbrutto"):LABEL = "Sålt brutto".
    BUFFER TT_VareSalg:HANDLE:BUFFER-FIELD("DBproc"):LABEL = "TB%".
/*     FIELD VerdiRabatt   AS  DECI LABEL "Rabatter netto" */
/*     FIELD GjenkjopAnt   AS  DECI LABEL "Returer"        */
/*     FIELD GjenkjopVerdi AS  DECI LABEL "Returer kr"     */
/*     FIELD LevKod        AS  CHAR LABEL "Levkod"         */
    BUFFER TT_SBudDW:HANDLE:BUFFER-FIELD("Butik"):LABEL = "Butik".
    BUFFER TT_SBudDW:HANDLE:BUFFER-FIELD("butnamn"):LABEL = "Namn".
    BUFFER TT_SBudDW:HANDLE:BUFFER-FIELD("DAGsalgbrutto"):LABEL = "Sålt dag".
    BUFFER TT_SBudDW:HANDLE:BUFFER-FIELD("DAGsalgBudsjett"):LABEL = IF cServicehandel <> "1" THEN "Budget dag"  ELSE "Fg. år dag".
    BUFFER TT_SBudDW:HANDLE:BUFFER-FIELD("UKEsalgbrutto"):LABEL = "Sålt vecka".
    BUFFER TT_SBudDW:HANDLE:BUFFER-FIELD("UKEsalgBudsjett"):LABEL = IF cServicehandel <> "1" THEN "Budget vecka" ELSE "Fg. år vecka".

    BUFFER TT_SBudMY:HANDLE:BUFFER-FIELD("Butik"):LABEL = "Butik".
    BUFFER TT_SBudMY:HANDLE:BUFFER-FIELD("butnamn"):LABEL = "Namn".
    BUFFER TT_SBudMY:HANDLE:BUFFER-FIELD("MNDsalgbrutto"):LABEL = "Sålt mån".
    BUFFER TT_SBudMY:HANDLE:BUFFER-FIELD("MNDsalgBudsjett"):LABEL = IF cServicehandel <> "1" THEN "Budget mån" ELSE "Fg. år mån".
    BUFFER TT_SBudMY:HANDLE:BUFFER-FIELD("AARsalgbrutto"):LABEL = "Sålt i år".
    BUFFER TT_SBudMY:HANDLE:BUFFER-FIELD("AARsalgBudsjett"):LABEL = IF cServicehandel <> "1" THEN "Budget år" ELSE "Fg. år år".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-PopulateDyn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PopulateDyn Procedure 
PROCEDURE PopulateDyn :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER TTname    AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER hTTHandle AS HANDLE      NO-UNDO.
DEFINE OUTPUT PARAMETER hDynTable AS HANDLE      NO-UNDO.
DEFINE INPUT PARAMETER cDynName AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER cFields   AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER cLabels   AS CHARACTER   NO-UNDO.


 /* Create an empty, undefined TEMP-TABLE */
CREATE TEMP-TABLE hDynTable.

/* Give it table?s fields & indexes */
DO ii = 1 TO NUM-ENTRIES(cFields):
     
    hDynTable:ADD-LIKE-FIELD(ENTRY(ii,cFields),TTname + "." + ENTRY(ii,cFields)).
    
/*     hDynTable:BUFFER-FIELD(ENTRY(ii,cFields)):LABEL = ENTRY(ii,cLabels). */
 END.
/* Add field like SalesRep.
RepName */
/* No more fields will be added */
hDynTable:TEMP-TABLE-PREPARE(cDynName).
 /* Get the buffer handle for the temp-table */
 hDynTable:COPY-TEMP-TABLE(hTTHandle,TRUE,FALSE,TRUE).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SalgsbudsjettDWFlik) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SalgsbudsjettDWFlik Procedure 
PROCEDURE SalgsbudsjettDWFlik :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
  ------------------------------------------------------------------------------*/
  DEFINE VARIABLE dDateLoop AS DATE        NO-UNDO.
  IF iSumbutNr > 0 THEN 
  DO:
      FIND FIRST bufTT_SBudDW WHERE 
           bufTT_SBudDW.Butik = iSumbutNr NO-ERROR.
      IF NOT AVAILABLE bufTT_SBudDW THEN 
         DO:                 
           CREATE bufTT_SBudDW.
           ASSIGN 
               bufTT_SBudDW.butik   = iSumbutNr
               bufTT_SBudDW.butnamn = 'Sum'.
         END.
  END.
  ASSIGN   
    dSumDBKrtmp    = 0  
    dSumVerdiSolgt = 0
    .

/*   BUDSJETT Butiker NO-LOCK: */
  DO iCount = 1 TO NUM-ENTRIES(cButListe):
      /* syspara: om 1 så får man se butiker som inte tillhör brgrp */
/*       IF cSeAllaiButStat <> "1" AND NOT CAN-DO(cButListe,STRING(Butiker.butik)) THEN */
/*           NEXT.                                                                      */
      FIND Butiker WHERE Butiker.butik = INT(ENTRY(icount,cButliste)) NO-LOCK NO-ERROR.      
      IF NOT AVAIL Butiker THEN
          NEXT.
      IF AVAILABLE TT_SBudDW THEN RELEASE TT_SBudDW.  
          
      FIND FIRST SBudHode NO-LOCK WHERE 
          SBudHode.ButikkNr = Butiker.Butik AND 
          SBudHode.Aar      = YEAR(dDato) AND 
          SBudHode.Aktiv    = TRUE NO-ERROR.
      /*    
      IF bLogg THEN 
          RUN bibl_loggDbFri.p (cErrLoggFil, 'onlinedatads.p: FillTT BudsjettHode '
                                + ' Butikk: ' + STRING(Butiker.Butik)
                                + ' Aar: ' + STRING(YEAR(dDato))
                                + ' Aktiv: ' + STRING(TRUE)
                                + ' Avail: ' + STRING(AVAILABLE SBudHode)
                                ).    
      */    
      ASSIGN iAntkunder = 0
             iAntkvitt  = 0.
      /* Dagen i dag */       
      FOR EACH StLinje WHERE 
          StLinje.Butik         = Butiker.Butik AND 
          StLinje.StTypeId      = 'BUTSTAT' AND
          StLinje.PerId         = 'DAG' AND 
          StLinje.AarPerLinNr   = iAarPerLinNr 
          USE-INDEX AarPerLinNr NO-LOCK:

         FIND FIRST TT_SBudDW WHERE 
             TT_SBudDW.Butik = Butiker.Butik NO-ERROR.
         IF NOT AVAILABLE TT_SBudDW THEN 
           DO:                 
             CREATE TT_SBudDW.
             ASSIGN 
                 TT_SBudDW.butik   = Butiker.butik
                 TT_SBudDW.butnamn = Butiker.butnamn.
           END.
         ASSIGN
           TT_SBudDW.DAGsalgbrutto    = StLinje.VerdiSolgt + StLinje.MvaVerdi.
      END.

      IF AVAILABLE SBudHode THEN
      FOR EACH SBudDag NO-LOCK WHERE 
               SBudDag.SBudId    = SBudHode.SBudId AND 
               SBudDag.AarMnd    = INT(STRING(YEAR(dDato),"9999") + STRING(MONTH(dDato),"99")) AND 
               SBudDag.AarMndDag = INT(STRING(YEAR(dDato),"9999") + STRING(MONTH(dDato),"99") + STRING(DAY(dDato),"99")):
         IF NOT AVAILABLE TT_SBudDW THEN 
             FIND FIRST TT_SBudDW WHERE 
                 TT_SBudDW.Butik = Butiker.Butik NO-ERROR.
         IF NOT AVAILABLE TT_SBudDW THEN 
           DO:                 
             CREATE TT_SBudDW.
             ASSIGN 
                 TT_SBudDW.butik   = Butiker.butik
                 TT_SBudDW.butnamn = Butiker.butnamn.
           END.
         ASSIGN
           TT_SBudDW.DAGSalgBudsjett  = TT_SBudDW.DAGSalgBudsjett + (IF AVAILABLE SBudDag THEN SBudDag.SalgBudsjett ELSE 0)
           TT_SBudDW.DAGSalgAvvikKr   = TT_SBudDW.DAGsalgbrutto - TT_SBudDW.DAGSalgBudsjett  
           TT_SBudDW.DAGSalgAvvikproc = ROUND((TT_SBudDW.DAGSalgAvvikKr / TT_SBudDW.DAGSalgBudsjett) * 100,2)
           .
      END.       
      
      /* Hitil denne uken */       
      FOR EACH StLinje WHERE 
          StLinje.Butik            = Butiker.Butik AND 
          StLinje.StTypeId      = 'BUTSTAT' AND
          StLinje.PerId         = 'DAG' AND 
          StLinje.AarPerLinNr   >= iAarPerLinNr - iWdOff AND  
          StLinje.AarPerLinNr   <= iAarPerLinNr  
          USE-INDEX AarPerLinNr NO-LOCK:
              
          IF NOT AVAILABLE TT_SBudDW THEN 
              FIND FIRST TT_SBudDW WHERE 
                  TT_SBudDW.Butik = Butiker.Butik NO-ERROR.
          IF NOT AVAILABLE TT_SBudDW THEN 
          DO:                 
              CREATE TT_SBudDW.
              ASSIGN 
                  TT_SBudDW.butik   = Butiker.butik
                  TT_SBudDW.butnamn = Butiker.butnamn.
          END.
          ASSIGN
            TT_SBudDW.UKEsalgbrutto    = TT_SBudDW.UKEsalgbrutto + StLinje.VerdiSolgt + StLinje.MvaVerdi
            .
      END.
      
      IF AVAILABLE SBudHode THEN DO dDateLoop = dDato - iWdOff TO dDato:
          OUTPUT TO "C:\tmp\onlbud.txt" APPEND.
              PUT UNFORMATTED STRING(dDato) " " iWdOff " " STRING(dDateLoop) SKIP.
          OUTPUT CLOSE.
          
          FOR EACH SBudDag NO-LOCK WHERE SBudDag.SBudId     = SBudHode.SBudId AND 
                                   SBudDag.AarMnd     = INT(STRING(YEAR(dDateLoop),"9999") + STRING(MONTH(dDateLoop),"99")) AND 
                                   SBudDag.AarMndDag  = INT(STRING(YEAR(dDateLoop),"9999") + STRING(MONTH(dDateLoop),"99") + STRING(DAY(dDateLoop),"99")):
/*              IF NOT AVAILABLE TT_SBudDW THEN */
             FIND FIRST TT_SBudDW WHERE TT_SBudDW.Butik = Butiker.Butik NO-ERROR.
             IF NOT AVAILABLE TT_SBudDW THEN DO:                 
                 CREATE TT_SBudDW.
                 ASSIGN TT_SBudDW.butik   = Butiker.butik
                        TT_SBudDW.butnamn = Butiker.butnamn.
             END.
             ASSIGN TT_SBudDW.UKESalgBudsjett  = TT_SBudDW.UKESalgBudsjett + (IF AVAILABLE SBudDag THEN SBudDag.SalgBudsjett ELSE 0)
                    TT_SBudDW.UKESalgAvvikKr   = TT_SBudDW.UKEsalgbrutto - TT_SBudDW.UKESalgBudsjett 
                    TT_SBudDW.UKESalgAvvikproc = ROUND((TT_SBudDW.UKESalgAvvikKr / TT_SBudDW.UKESalgBudsjett) * 100,2).

          END.

/*  * innan loop     FOR EACH SBudDag NO-LOCK WHERE SBudDag.SBudId     = SBudHode.SBudId AND                                                                            */
/*  * innan loop                              SBudDag.AarMnd     = INT(STRING(YEAR(dDato),"9999") + STRING(MONTH(dDato),"99")) AND                                      */
/*  * innan loop                              SBudDag.AarMndDag >= INT(STRING(YEAR(dDato),"9999") + STRING(MONTH(dDato),"99") +  STRING(DAY(dDato -  iWdOff),"99")) AND */
/*  * innan loop                              SBudDag.AarMndDag <= INT(STRING(YEAR(dDato),"9999") + STRING(MONTH(dDato),"99") + STRING(DAY(dDato),"99")):               */
/*  * innan loop        IF NOT AVAILABLE TT_SBudDW THEN                                                                                                                 */
/*  * innan loop            FIND FIRST TT_SBudDW WHERE TT_SBudDW.Butik = Butiker.Butik NO-ERROR.                                                                        */
/*  * innan loop        IF NOT AVAILABLE TT_SBudDW THEN DO:                                                                                                             */
/*  * innan loop            CREATE TT_SBudDW.                                                                                                                           */
/*  * innan loop            ASSIGN TT_SBudDW.butik   = Butiker.butik                                                                                                    */
/*  * innan loop                   TT_SBudDW.butnamn = Butiker.butnamn.                                                                                                 */
/*  * innan loop        END.                                                                                                                                            */
/*  * innan loop        ASSIGN TT_SBudDW.UKESalgBudsjett  = TT_SBudDW.UKESalgBudsjett + (IF AVAILABLE SBudDag THEN SBudDag.SalgBudsjett ELSE 0)                         */
/*  * innan loop               TT_SBudDW.UKESalgAvvikKr   = TT_SBudDW.UKEsalgbrutto - TT_SBudDW.UKESalgBudsjett                                                         */
/*  * innan loop               TT_SBudDW.UKESalgAvvikproc = ROUND((TT_SBudDW.UKESalgAvvikKr / TT_SBudDW.UKESalgBudsjett) * 100,2).                                      */


/*       IF bLogg THEN                                                                          */
/*           RUN bibl_loggDbFri.p (cErrLoggFil, 'onlinedatads.p: Hitil i uken '                 */
/*                                 + ' BudId: '  + STRING(SBudDag.SBudId)                       */
/*                                 + ' AaMnd: '  + STRING(SBudDag.AarMnd)                       */
/*                                 + ' AaMndDag: '  + STRING(SBudDag.AarMndDag)                 */
/*                                 + ' Butikk: ' + STRING(TT_SBudDW.butik)                      */
/*                                 + ' Navn: '   + TT_SBudDW.butnamn                            */
/*                                 + ' UKESalgBudsjett: '  + STRING(TT_SBudDW.UKESalgBudsjett)  */
/*                                 + ' UKESalgAvvikKr: '   + STRING(TT_SBudDW.UKESalgAvvikKr)   */
/*                                 + ' UKESalgAvvikproc: ' + STRING(TT_SBudDW.UKESalgAvvikproc) */
/*                                 ).                                                           */
      END.       
  END.
  
  IF AVAILABLE bufTT_SBudDW THEN
  TOTALER: 
  DO:
      FOR EACH TT_SBudDW WHERE 
        TT_SBudDW.butik < bufTT_SBudDW.Butik:
            ASSIGN 
                bufTT_SBudDW.DAGsalgbrutto   = bufTT_SBudDW.DAGsalgbrutto   + TT_SBudDW.DAGsalgbrutto
                bufTT_SBudDW.UKEsalgbrutto   = bufTT_SBudDW.UKEsalgbrutto   + TT_SBudDW.UKEsalgbrutto
                bufTT_SBudDW.DAGSalgBudsjett = bufTT_SBudDW.DAGSalgBudsjett + TT_SBudDW.DAGSalgBudsjett
                bufTT_SBudDW.UKESalgBudsjett = bufTT_SBudDW.UKESalgBudsjett + TT_SBudDW.UKESalgBudsjett
                bufTT_SBudDW.DAGSalgAvvikKr  = bufTT_SBudDW.DAGSalgAvvikKr  + TT_SBudDW.DAGSalgAvvikKr
                bufTT_SBudDW.UKESalgAvvikKr  = bufTT_SBudDW.UKESalgAvvikKr  + TT_SBudDW.UKESalgAvvikKr 
                .
      END.
      ASSIGN 
           bufTT_SBudDW.DAGSalgAvvikproc = ROUND((bufTT_SBudDW.DAGSalgAvvikKr / bufTT_SBudDW.DAGSalgBudsjett) * 100,2)
           bufTT_SBudDW.UKESalgAvvikproc = ROUND((bufTT_SBudDW.UKESalgAvvikKr / bufTT_SBudDW.UKESalgBudsjett) * 100,2)
           .            
  END. /* TOTALER */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SalgsbudsjettMYFlik) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SalgsbudsjettMYFlik Procedure 
PROCEDURE SalgsbudsjettMYFlik :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

  IF iSumbutNr > 0 THEN 
  DO:
      FIND FIRST bufTT_SBudMY WHERE 
                 bufTT_SBudMY.Butik = iSumbutNr NO-ERROR.
      IF NOT AVAILABLE bufTT_SBudMY THEN 
        DO:                 
          CREATE bufTT_SBudMY.
          ASSIGN 
              bufTT_SBudMY.butik   = iSumbutNr
              bufTT_SBudMY.butnamn = 'Sum'.
        END.
  END.
  ASSIGN   
    dSumDBKrtmp    = 0  
    dSumVerdiSolgt = 0
    .

/*   BUDSJETT Butiker NO-LOCK: */
  DO iCount = 1 TO NUM-ENTRIES(cButListe):
      /* syspara: om 1 så får man se butiker som inte tillhör brgrp */
/*       IF cSeAllaiButStat <> "1" AND NOT CAN-DO(cButListe,STRING(Butiker.butik)) THEN */
/*           NEXT.                                                                      */
      FIND Butiker WHERE Butiker.butik = INT(ENTRY(icount,cButliste)) NO-LOCK NO-ERROR.      
      IF NOT AVAIL Butiker THEN
          NEXT.

      IF AVAILABLE TT_SBudMY THEN RELEASE TT_SBudMY.  
          
      FIND FIRST SBudHode NO-LOCK WHERE 
          SBudHode.ButikkNr = Butiker.Butik AND 
          SBudHode.Aar      = YEAR(dDato) AND 
          SBudHode.Aktiv    = TRUE NO-ERROR.

      /* Hitil i måneden */       
      FOR EACH StLinje WHERE 
          StLinje.Butik         = Butiker.Butik AND 
          StLinje.StTypeId      = 'BUTSTAT' AND
          StLinje.PerId         = 'DAG' AND 
                                   /* År */             /* Første dag i mnd    */   /* Første dag i året      */
          StLinje.AarPerLinNr   >= YEAR(dDato) * 1000 + ((dDato - DAY(dDato) + 1) - DATE(12,31,YEAR(dDato) - 1)) AND 
                                   /* Dagens dato */
          StLinje.AarPerLinNr   <= iAarPerLinNr  
          USE-INDEX AarPerLinNr NO-LOCK:
         IF NOT AVAILABLE TT_SBudMY THEN 
             FIND FIRST TT_SBudMY WHERE 
                 TT_SBudMY.Butik = Butiker.Butik NO-ERROR.
         IF NOT AVAILABLE TT_SBudMY THEN 
           DO:                 
             CREATE TT_SBudMY.
             ASSIGN 
                 TT_SBudMY.butik   = Butiker.butik
                 TT_SBudMY.butnamn = Butiker.butnamn.
           END.
         ASSIGN
           TT_SBudMY.MNDsalgbrutto    = TT_SBudMY.MNDsalgbrutto + StLinje.VerdiSolgt + StLinje.MvaVerdi
           .
      END.
      
      IF AVAILABLE SBudHode THEN
      FOR EACH SBudDag NO-LOCK WHERE 
               SBudDag.SBudId    = SBudHode.SBudId AND 
               SBudDag.AarMnd    = INT(STRING(YEAR(dDato),"9999") + STRING(MONTH(dDato),"99")) AND 
               SBudDag.AarMndDag >= INT(STRING(YEAR(dDato),"9999") + STRING(MONTH(dDato),"99") + '01') AND 
               SBudDag.AarMndDag <= INT(STRING(YEAR(dDato),"9999") + STRING(MONTH(dDato),"99") + STRING(DAY(dDato),"99")):
         IF NOT AVAILABLE TT_SBudMY THEN 
             FIND FIRST TT_SBudMY WHERE 
                 TT_SBudMY.Butik = Butiker.Butik NO-ERROR.
         IF NOT AVAILABLE TT_SBudMY THEN 
           DO:                 
             CREATE TT_SBudMY.
             ASSIGN 
                 TT_SBudMY.butik   = Butiker.butik
                 TT_SBudMY.butnamn = Butiker.butnamn.
           END.         
         ASSIGN
           TT_SBudMY.MNDSalgBudsjett  = TT_SBudMY.MNDSalgBudsjett + (IF AVAILABLE SBudDag THEN SBudDag.SalgBudsjett ELSE 0)
           TT_SBudMY.MNDSalgAvvikKr   = TT_SBudMY.MNDsalgbrutto - TT_SBudMY.MNDSalgBudsjett
           TT_SBudMY.MNDSalgAvvikproc = ROUND((TT_SBudMY.MNDSalgAvvikKr / TT_SBudMY.MNDSalgBudsjett) * 100,2)
           .
      IF bLogg THEN 
          RUN bibl_loggDbFri.p (cErrLoggFil, 'onlinedatads.p: Hitil i måneden '
                                + ' BudId: '  + STRING(SBudDag.SBudId)
                                + ' AaMnd: '  + STRING(SBudDag.AarMnd)
                                + ' AaMndDag: '  + STRING(SBudDag.AarMndDag)
                                + ' Butikk: ' + STRING(TT_SBudMY.butik)
                                + ' Navn: '   + TT_SBudMY.butnamn
                                + ' MNDSalgBudsjett: '  + STRING(TT_SBudMY.MNDSalgBudsjett)
                                + ' MNDSalgAvvikKr: '   + STRING(TT_SBudMY.MNDSalgAvvikKr)
                                + ' MNDSalgAvvikproc: ' + STRING(TT_SBudMY.MNDSalgAvvikproc)
                                ).    
      END.       
           
      /* Hitil dette år */       
      FOR EACH StLinje WHERE 
          StLinje.Butik         = Butiker.Butik AND 
          StLinje.StTypeId      = 'BUTSTAT' AND
          StLinje.PerId         = 'DAG' AND 
          StLinje.AarPerLinNr   >= YEAR(dDato) * 1000 + 1 AND 
                                   /* Dagens dato */
          StLinje.AarPerLinNr   <= iAarPerLinNr  
          USE-INDEX AarPerLinNr NO-LOCK:
         IF NOT AVAILABLE TT_SBudMY THEN 
             FIND FIRST TT_SBudMY WHERE 
                 TT_SBudMY.Butik = Butiker.Butik NO-ERROR.
         IF NOT AVAILABLE TT_SBudMY THEN 
           DO:                 
             CREATE TT_SBudMY.
             ASSIGN 
                 TT_SBudMY.butik   = Butiker.butik
                 TT_SBudMY.butnamn = Butiker.butnamn.
           END.
         ASSIGN
           TT_SBudMY.AARsalgbrutto    = TT_SBudMY.AARsalgbrutto + StLinje.VerdiSolgt + StLinje.MvaVerdi
           .
      END.
      
      IF AVAILABLE SBudHode THEN
      FOR EACH SBudDag NO-LOCK WHERE 
               SBudDag.SBudId    = SBudHode.SBudId AND 
               SBudDag.AarMnd    >= INT(STRING(YEAR(dDato),"9999") + '01') AND 
               SBudDag.AarMnd    <= INT(STRING(YEAR(dDato),"9999") + STRING(MONTH(dDato),"99")) AND 
               SBudDag.AarMndDag >= INT(STRING(YEAR(dDato),"9999") + '0101') AND 
               SBudDag.AarMndDag <= INT(STRING(YEAR(dDato),"9999") + STRING(MONTH(dDato),"99") + STRING(DAY(dDato),"99")):
         
         IF NOT AVAILABLE TT_SBudMY THEN 
             FIND FIRST TT_SBudMY WHERE 
                 TT_SBudMY.Butik = Butiker.Butik NO-ERROR.
         IF NOT AVAILABLE TT_SBudMY THEN 
           DO:                 
             CREATE TT_SBudMY.
             ASSIGN 
                 TT_SBudMY.butik   = Butiker.butik
                 TT_SBudMY.butnamn = Butiker.butnamn.
           END.
         
         ASSIGN
           TT_SBudMY.AARSalgBudsjett  = TT_SBudMY.AARSalgBudsjett + (IF AVAILABLE SBudDag THEN SBudDag.SalgBudsjett ELSE 0)
           TT_SBudMY.AARSalgAvvikKr   = TT_SBudMY.AARsalgbrutto - TT_SBudMY.AARSalgBudsjett
           TT_SBudMY.AARSalgAvvikproc = ROUND((TT_SBudMY.AARSalgAvvikKr / TT_SBudMY.AARSalgBudsjett) * 100,2)
           .
      IF bLogg THEN 
          RUN bibl_loggDbFri.p (cErrLoggFil, 'onlinedatads.p: Hitil i året '
                                + ' BudId: '  + STRING(SBudDag.SBudId)
                                + ' AaMnd: '  + STRING(SBudDag.AarMnd)
                                + ' AaMndDag: '  + STRING(SBudDag.AarMndDag)
                                + ' Butikk: ' + STRING(TT_SBudMY.butik)
                                + ' Navn: '   + TT_SBudMY.butnamn
                                + ' AARSalgBudsjett: '  + STRING(TT_SBudMY.AARSalgBudsjett)
                                + ' AARSalgAvvikKr: '   + STRING(TT_SBudMY.AARSalgAvvikKr)
                                + ' AARSalgAvvikproc: ' + STRING(TT_SBudMY.AARSalgAvvikproc)
                                ).    
      END.
      
             
  END.
  
  IF AVAILABLE bufTT_SBudMY THEN
  AKKUMULERING: 
  DO:
      FOR EACH TT_SBudMY WHERE 
          TT_SBudMY.Butik < bufTT_SBudMY.butik:
          ASSIGN 
               bufTT_SBudMY.MNDsalgbrutto    = bufTT_SBudMY.MNDsalgbrutto   + TT_SBudMY.MNDsalgbrutto
               bufTT_SBudMY.AARsalgbrutto    = bufTT_SBudMY.AARsalgbrutto   + TT_SBudMY.AARsalgbrutto
               bufTT_SBudMY.MNDSalgBudsjett  = bufTT_SBudMY.MNDSalgBudsjett + TT_SBudMY.MNDSalgBudsjett
               bufTT_SBudMY.MNDSalgAvvikKr   = bufTT_SBudMY.MNDSalgAvvikKr  + TT_SBudMY.MNDSalgAvvikKr
               bufTT_SBudMY.AARSalgBudsjett  = bufTT_SBudMY.AARSalgBudsjett + TT_SBudMY.AARSalgBudsjett
               bufTT_SBudMY.AARSalgAvvikKr   = bufTT_SBudMY.AARSalgAvvikKr  + TT_SBudMY.AARSalgAvvikKr
            .
      END.

      ASSIGN       
           bufTT_SBudMY.MNDSalgAvvikproc = ROUND((bufTT_SBudMY.MNDSalgAvvikKr / bufTT_SBudMY.MNDSalgBudsjett) * 100,2)
           bufTT_SBudMY.AARSalgAvvikproc = ROUND((bufTT_SBudMY.AARSalgAvvikKr / bufTT_SBudMY.AARSalgBudsjett) * 100,2)
           .
      
  END. /* AKKUMULERING */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SelgerFlik) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SelgerFlik Procedure 
PROCEDURE SelgerFlik :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/
  DEFINE VARIABLE dAntUtens       AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dSumUtensBrutto AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE i2 AS INTEGER     NO-UNDO.
  DEFINE VARIABLE cDataObjekt AS CHARACTER   NO-UNDO.
  DEFINE BUFFER bufStlinje FOR StLinje.
  DO iCount = 1 TO 1 /* NUM-ENTRIES(cButListe) */:
      FOR EACH StLinje WHERE StLinje.Butik = iButikknr /* INT(ENTRY(icount,cButliste)) */ AND StLinje.StTypeId = 'SELGER' AND
          StLinje.PerId = 'DAG' AND StLinje.AarPerLinNr = iAarPerLinNr /* AND StLinje.VerdiSolgt <> 0 */ USE-INDEX AarPerLinNr NO-LOCK:
          
          /* Skal ikke ha med selgere som ikke har solgt. */
          IF StLinje.VerdiSolgt = 0 AND StLinje.ReklVerdi = 0 AND StLinje.GjenkjopVerdi = 0 THEN 
            NEXT.
          
          FIND TT_Selger WHERE TT_Selger.selgernr = INTE(StLinje.DataObjekt) NO-ERROR.
          IF NOT AVAIL TT_Selger THEN DO:
              FIND Selger WHERE Selger.selgernr = INTE(StLinje.DataObjekt) NO-LOCK NO-ERROR.
              CREATE TT_Selger.
              ASSIGN
                TT_Selger.selgernr  = INT(StLinje.DataObjekt)
                TT_Selger.navn      = IF AVAIL Selger THEN selger.NavnIKasse ELSE "?? " + StLinje.Beskrivelse.
          END.
          IF cHitrate = "1" THEN DO:
            ASSIGN dAntUtens       = 0
                   dSumUtensBrutto = 0.
            DO i2 = 1 TO NUM-ENTRIES(cVgList):
                FOR EACH bufStlinje WHERE bufStLinje.Butik = iButikknr AND 
                                          bufStlinje.sttypeid   = "SELGER-VG"    AND
                                          bufStlinje.perid      = "DAG"       AND
                                          bufStlinje.aarperlinnr = iAarPerLinNr AND
                                          bufStlinje.dataobjekt = StLinje.Dataobjekt + CHR(1) + STRING(INT(ENTRY(i2,cVgList)),"999999")
                                                                USE-INDEX AarPerLinNr NO-LOCK:

                    ASSIGN dAntUtens       = dAntUtens    + bufStlinje.antsolgt
                           dSumUtensBrutto = dSumUtensBrutto + bufStlinje.VerdiSolgt + bufStlinje.MvaVerdi.
/*                     ASSIGN tt_hitrate.antutens    = tt_hitrate.antutens    + stlinje.antsolgt                       */
/*                            tt_hitrate.utensbrutto = tt_hitrate.utensbrutto + StLinje.VerdiSolgt + StLinje.MvaVerdi. */
                END.
            END.

          END.
          ASSIGN
            TT_Selger.salgbrutto  = TT_Selger.salgbrutto    + StLinje.VerdiSolgt + StLinje.MvaVerdi
/*             TT_Selger.VerdiSolgt  = TT_Selger.VerdiSolgt    + StLinje.VerdiSolgt */
            TT_Selger.AntSolgt    = TT_Selger.AntSolgt      + StLinje.AntSolgt
            TT_Selger.VerdiRabatt = TT_Selger.VerdiRabatt   + StLinje.VerdiRabatt
/*             TT_Selger.DBkr        = TT_Selger.DBkr          + StLinje.VerdiSolgt - StLinje.VVarekost */
              dDBKrtmp            =  StLinje.VerdiSolgt - StLinje.VVarekost
              TT_Selger.DBproc    = IF StLinje.VerdiSolgt <= 0 THEN 0 ELSE ROUND(dDBKrtmp / StLinje.VerdiSolgt * 100,2)
/*             TT_Selger.mvaverdi      = TT_Selger.mvaverdi      + StLinje.MvaVerdi */
/*             TT_Selger.vvarekost     = TT_Selger.vvarekost     + StLinje.VVarekost */
            TT_Selger.GjenkjopAnt   = TT_Selger.GjenkjopAnt   + StLinje.GjenkjopAnt
            TT_Selger.GjenkjopVerdi = TT_Selger.GjenkjopVerdi + StLinje.GjenkjopVerdi.
          IF dAntUtens > 0 AND TT_Selger.AntSolgt > 0 THEN
/*               ASSIGN TT_Selger.AntUtens = TT_Selger.AntUtens + dAntUtens                    */
/*                      TT_Selger.SumUtensBrutto = TT_Selger.SumUtensBrutto + dSumUtensBrutto. */
              ASSIGN TT_Selger.Hitrate = ROUND(dAntUtens / TT_Selger.AntSolgt * 100,1)
                      TT_Selger.Merfsg  = ROUND(dSumUtensBrutto / TT_Selger.salgbrutto * 100,1).

      END.
  END.
  DO iCount = 1 TO 1 /* NUM-ENTRIES(cButListe) */:
      FOR EACH kasse NO-LOCK WHERE kasse.butikknr = iButikknr /* INT(ENTRY(icount,cButliste)) */  AND kasse.kassenr < 99:
          FOR EACH Bonghode WHERE bonghode.butikknr = kasse.butikknr AND
                                  bonghode.gruppenr = 1              AND
                                  bonghode.kassenr  = kasse.kassenr  AND
                                  bonghode.dato     = dDato        NO-LOCK:
              IF bonghode.belop <= 0 OR bonghode.makulert = 2 THEN
                  NEXT.
              IF CAN-FIND(FIRST bonglinje WHERE bonglinje.b_id = bonghode.b_id AND bonglinje.ttid = 1) THEN DO:
                  FIND TT_Selger WHERE TT_Selger.SelgerNr = bonghode.selgernr NO-ERROR.
                  IF AVAIL TT_Selger THEN
                      TT_Selger.AntKunder = TT_Selger.AntKunder + 1.
              END.
          END.
      END.
  END.
/*   FOR EACH TT_Selger WHERE TT_Selger.AntUtens > 0:                                               */
/*       ASSIGN TT_Selger.Hitrate = ROUND(TT_Selger.AntUtens / TT_Selger.AntSolgt * 100,1)          */
/*              TT_Selger.Merfsg  = ROUND(TT_Selger.SumUtensBrutto / TT_Selger.salgbrutto * 100,1). */
/*                                                                                                  */
/*   END.                                                                                           */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaDynTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaDynTT Procedure 
PROCEDURE SkapaDynTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    RUN populateDyn ("TT_Avdeling",INPUT BUFFER TT_Avdeling:HANDLE,OUTPUT httAvdeling,"Avdeling",cAvdFelter,cAvdLabels).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-VaregruppeFlik) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE VaregruppeFlik Procedure 
PROCEDURE VaregruppeFlik :
/*------------------------------------------------------------------------------
                        Purpose:                                                                                                                                          
                        Notes:                                                                                                                                            
        ------------------------------------------------------------------------------*/

  DO iCount = 1 TO 1 /* NUM-ENTRIES(cButListe) */ :
      FOR EACH StLinje WHERE StLinje.Butik = iButikknr /* INT(ENTRY(icount,cButliste)) */ AND StTypeId = 'VAREGR' AND
          PerId = 'DAG' AND AarPerLinNr = iAarPerLinNr /* AND StLinje.VerdiSolgt <> 0 */ USE-INDEX AarPerLinNr NO-LOCK:
          FIND TT_Vg WHERE TT_Vg.vg = INT(StLinje.DataObjekt) NO-ERROR.
          IF NOT AVAIL TT_Vg THEN DO:
              FIND VarGr WHERE VarGr.Vg = INT(StLinje.DataObjekt) NO-LOCK NO-ERROR.
              CREATE TT_Vg.
              ASSIGN
                TT_Vg.vg            = INT(StLinje.DataObjekt)
                TT_Vg.VgBeskr       = IF AVAIL VarGr THEN VarGr.vgbeskr ELSE "?? " + StLinje.Beskrivelse.
          END.
          ASSIGN
            TT_Vg.salgbrutto    = TT_Vg.salgbrutto  + StLinje.VerdiSolgt + StLinje.MvaVerdi
/*             TT_Vg.VerdiSolgt    = TT_Vg.VerdiSolgt  + StLinje.VerdiSolgt */
            TT_Vg.AntSolgt      = TT_Vg.AntSolgt    + StLinje.AntSolgt
            TT_Vg.VerdiRabatt   = TT_Vg.VerdiRabatt + StLinje.VerdiRabatt
            dDBKrtmp            =  StLinje.VerdiSolgt - StLinje.VVarekost
            TT_Vg.DBproc        = IF StLinje.VerdiSolgt <= 0 THEN 0 ELSE ROUND(dDBKrtmp / StLinje.VerdiSolgt * 100,2)
/*             TT_Vg.mvaverdi      = TT_Vg.mvaverdi      + StLinje.MvaVerdi */
/*             TT_Vg.vvarekost     = TT_Vg.vvarekost     + StLinje.VVarekost */
            TT_Vg.GjenkjopAnt   = TT_Vg.GjenkjopAnt   + StLinje.GjenkjopAnt
            TT_Vg.GjenkjopVerdi = TT_Vg.GjenkjopVerdi + StLinje.GjenkjopVerdi.
      END.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

