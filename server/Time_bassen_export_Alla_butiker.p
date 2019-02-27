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

def var iMan as inte no-undo.
def var cAntDag as char no-undo.

DEFINE VARIABLE iLastReportMonth   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iReportYear        AS INTEGER     NO-UNDO.
DEFINE VARIABLE dLastDayFeb        AS DATE        NO-UNDO.
DEFINE VARIABLE iAarPerlinNr       AS INTEGER     NO-UNDO.
DEFINE VARIABLE dRunDay            AS DATE        NO-UNDO.
DEFINE VARIABLE dDatoFra           AS DATE        NO-UNDO.
DEFINE VARIABLE dDatoTil           AS DATE        NO-UNDO.
DEFINE VARIABLE dDatoLoop          AS DATE        NO-UNDO.
DEFINE VARIABLE cFil_01            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFil_02            AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ii  AS INTEGER     NO-UNDO.

def stream time1.
def stream time2.


define temp-table tt_time1 no-undo
field butik AS INTE
field ar AS INTE
field man AS INTE
field Var_salgsum AS DECI DECIMALS 2
field Tj_salgsum AS DECI  DECIMALS 2
field Tj_tbsum AS DECI    DECIMALS 2
field Var_tbsum AS DECI   DECIMALS 2
field Tot_tb as deci      DECIMALS 2
field kundeant AS INTE
field vareant  AS DECI
field VpK_salgsum AS DECI DECIMALS 2
field TpK_salgsum AS DECI DECIMALS 2
field ApK_vareant AS DECI DECIMALS 2
field KpD_ant AS DECI
field VpD_salgsum AS DECI DECIMALS 2
FIELD non_salesum AS DECI DECIMALS 2
    INDEX bam IS PRIMARY butik ar man.

define temp-table tt_time2 no-undo
field butik AS INTE
field ar AS INTE
field man AS INTE
field avd AS INTE
field salgsum2 AS DECI   DECIMALS 2
field tbsum2 AS DECI     DECIMALS 2
field TBprocent2 AS DECI DECIMALS 2
field tj_salgsum2 AS DECI DECIMALS 2
field tj_tbsum2 AS DECI   DECIMALS 2
field tj_TBprocent2 AS DECI DECIMALS 2
    INDEX bama IS PRIMARY butik ar man avd.

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

ASSIGN dRunDay = TODAY.

IF MONTH(dRunDay) = 1 THEN DO:
    ASSIGN iReportYear = YEAR(dRunDay) - 1
           iLastReportMonth = 12.
END.
ELSE DO:
    ASSIGN iReportYear = YEAR(dRunDay)
           iLastReportMonth = MONTH(dRunDay) - 1.
END.
ASSIGN dLastDayFeb = DATE(3,1,iReportYear) - 1
       iMan        = 0
       cAntDag     = "31," + STRING(DAY(dLastDayFeb)) + ",31,30,31,30,31,31,30,31,30,31".


RUN Rapport.
ASSIGN cFil_01 = "C:\home\lindbak\sendes\Bassem\time_" + STRING(iReportYear,"9999") + "_01.csv"
       cFil_02 = "C:\home\lindbak\sendes\Bassem\time_" + STRING(iReportYear,"9999") + "_02.csv".
output stream time1 to VALUE(cFil_01).
output stream time2 to VALUE(cFil_02).

for each tt_time1:
    IF tt_time1.butik         = ? THEN tt_time1.butik         = 0.
    IF tt_time1.ar            = ? THEN tt_time1.ar            = 0.
    IF tt_time1.man           = ? THEN tt_time1.man           = 0.
    IF tt_time1.Var_salgsum   = ? THEN tt_time1.Var_salgsum   = 0.
    IF tt_time1.Tj_salgsum    = ? THEN tt_time1.Tj_salgsum    = 0.
    IF tt_time1.Var_tbsum  = ? THEN tt_time1.Var_tbsum  = 0.
    IF tt_time1.Tj_tbsum   = ? THEN tt_time1.Tj_tbsum   = 0.
    IF tt_time1.Tot_tb        = ? THEN tt_time1.Tot_tb        = 0.
    IF tt_time1.kundeant      = ? THEN tt_time1.kundeant      = 0.
    IF tt_time1.vareant       = ? THEN tt_time1.vareant       = 0.
    IF tt_time1.VpK_salgsum   = ? THEN tt_time1.VpK_salgsum   = 0.
    IF tt_time1.TpK_salgsum   = ? THEN tt_time1.TpK_salgsum   = 0.
    IF tt_time1.ApK_vareant   = ? THEN tt_time1.ApK_vareant   = 0.
    IF tt_time1.KpD_ant       = ? THEN tt_time1.KpD_ant       = 0.
    IF tt_time1.VpD_salgsum   = ? THEN tt_time1.VpD_salgsum   = 0.
    IF tt_time1.non_salesum   = ? THEN tt_time1.non_salesum   = 0.
    export    stream time1 delimiter ";" tt_time1.
end.

for each tt_time2:
   IF tt_time2.butik         = ? THEN tt_time2.butik         = 0.
   IF tt_time2.ar            = ? THEN tt_time2.ar            = 0.
   IF tt_time2.man           = ? THEN tt_time2.man           = 0.
   IF tt_time2.avd           = ? THEN tt_time2.avd           = 0.
   IF tt_time2.salgsum2      = ? THEN tt_time2.salgsum2      = 0.
   IF tt_time2.tbsum2        = ? THEN tt_time2.tbsum2        = 0.
   IF tt_time2.TBprocent2    = ? THEN tt_time2.TBprocent2    = 0.
   IF tt_time2.tj_salgsum2   = ? THEN tt_time2.tj_salgsum2   = 0.
   IF tt_time2.tj_tbsum2     = ? THEN tt_time2.tj_tbsum2     = 0.
   IF tt_time2.tj_TBprocent2 = ? THEN tt_time2.tj_TBprocent2 = 0.
   export stream time2 delimiter ";" tt_time2.
end.

output stream time1 close.
output stream time2 close.


QUIT.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-KundVaror) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KundVaror Procedure 
PROCEDURE KundVaror :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iButik     AS INTEGER     NO-UNDO.
    DEFINE INPUT  PARAMETER dFra       AS DATE        NO-UNDO.
    DEFINE INPUT  PARAMETER dTil       AS DATE        NO-UNDO.
    DEFINE OUTPUT PARAMETER dAntKunder AS DECIMAL     NO-UNDO.
    DEFINE OUTPUT PARAMETER dAntvaror  AS DECIMAL     NO-UNDO.
    DEFINE        VARIABLE  lFunnet    AS LOGICAL     NO-UNDO.
    DEFINE        VARIABLE  dDato      AS DATE        NO-UNDO.
    IF YEAR(dFra) = 2010 AND MONTH(dFra) < 5 THEN DO:
        FOR EACH akt_rapp WHERE akt_rapp.dato = dFra AND butik = iButik NO-LOCK:
            assign dAntKunder = akt_rapp.ant_kunder
                   dAntvaror  = akt_rapp.oms_ant.
        END.
    END.
    ELSE DO:
        FOR EACH kasse WHERE kasse.butik = iButik NO-LOCK:
            DO dDato = dFra TO dTil:
                FOR EACH bonghode WHERE bonghode.butik   = iButik AND 
                                        bonghode.gruppe  = 1      AND
                                        bonghode.kassenr = kasse.kassenr AND
                                        bonghode.dato    = dDato NO-LOCK.
                    IF bonghode.makulert = 2 THEN
                        NEXT.
                    lFunnet = FALSE.
                    FOR EACH bonglinje WHERE bonglinje.b_id = bonghode.b_id AND CAN-DO("1,3,10",STRING(bonglinje.ttid)) NO-LOCK:
                        IF bonglinje.makulert = TRUE THEN
                            NEXT.
                        lFunnet = TRUE.
                        ASSIGN dAntVaror = dAntVaror + bonglinje.antall.
                    END.
                    IF lFunnet = TRUE THEN
                        dAntKunder = dAntKunder + 1.
                END.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Rapport) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rapport Procedure 
PROCEDURE Rapport :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iAntkunder AS INTEGER     NO-UNDO.
DEFINE VARIABLE dAntvaror  AS DECIMAL     NO-UNDO.
REPEAT iMan = 1 to iLastReportMonth:
    ASSIGN iAarPerlinNr = int(STRING(iReportYear,"9999") + STRING(iMan,"999")).
    for each butiker where butiker.butik > 100 no-lock:
        IF NOT CAN-FIND(FIRST StLinje WHERE StLinje.butik = butiker.butik) THEN
            NEXT.
        create tt_time1.
        assign tt_time1.butik = butiker.butik
               tt_time1.Ar    = iReportYear
               tt_time1.man   = iMan.
    
        FOR EACH StLinje WHERE StLinje.butik = butiker.butik AND StLinje.sttypeid = "AVDELING" AND 
                               StLinje.perid = "MANED"       AND StLinje.aarperlinnr = iAarperlinnr NO-LOCK:
/*             FIND VarGr WHERE Vargr.vg = INT(StLinje.Dataobjekt) NO-LOCK NO-ERROR. */
/*             IF NOT AVAIL vargr THEN                                               */
/*                 NEXT.                                                             */
/*             FIND huvgr OF vargr NO-LOCK NO-ERROR.                                 */
/*             IF NOT AVAIL huvgr THEN                                               */
/*                 NEXT.                                                             */
            create tt_time2.
            assign tt_time2.butik = butiker.butik
                   tt_time2.Ar    = iReportYear
                   tt_time2.man   = iMan
                   tt_time2.avd   = INT(StLinje.dataobjekt).

            assign
                tt_time1.Var_salgsum  = tt_time1.Var_salgsum + if INT(StLinje.Dataobjekt) <> 13 then StLinje.VerdiSolgt else 0
                tt_time1.Tj_salgsum   = tt_time1.Tj_salgsum  + if INT(StLinje.Dataobjekt)  = 13 then StLinje.VerdiSolgt else 0
                tt_time1.Tj_tbsum     = tt_time1.Tj_tbsum    + if INT(StLinje.Dataobjekt)  = 13 then (StLinje.VerdiSolgt - StLinje.VVarekost) else 0
                tt_time1.Var_tbsum    = tt_time1.Var_tbsum   + if INT(StLinje.Dataobjekt)  <> 13 then (StLinje.VerdiSolgt - StLinje.VVarekost) else 0
                tt_time1.Tot_tb       = tt_time1.Tot_tb      + (StLinje.VerdiSolgt - StLinje.VVarekost).
            
            assign
                tt_time2.salgsum2      = tt_time2.salgsum2    + StLinje.VerdiSolgt
                tt_time2.tbsum2        = tt_time2.tbsum2      + StLinje.VerdiSolgt - StLinje.VVarekost
                tt_time2.tj_salgsum2   = tt_time2.tj_salgsum2 + if INT(StLinje.Dataobjekt)  = 13 then StLinje.VerdiSolgt else 0
                tt_time2.tj_tbsum2     = tt_time2.tj_tbsum2   + if INT(StLinje.Dataobjekt)  = 13 then (StLinje.VerdiSolgt - StLinje.VVarekost) else 0
                .

        end. 
    end.
END. /* REPEAT */

for each tt_time1:
    ASSIGN dDatoFra = DATE(tt_time1.man,1,tt_time1.ar).
    IF tt_time1.man = 12 THEN
        ASSIGN dDatoTil = DATE(tt_time1.man,31,tt_time1.ar).
    ELSE
        ASSIGN dDatoTil = DATE(tt_time1.man + 1,1,tt_time1.ar) - 1.
    iAntkunder = 0.
    dAntvaror  = 0.
    RUN KundVaror (tt_time1.butik,dDatoFra,dDatoTil,OUTPUT iAntkunder,OUTPUT dAntvaror).
    assign tt_time1.kundeant = tt_time1.kundeant + iAntkunder
           tt_time1.vareant  = tt_time1.vareant  + ROUND(dAntvaror,0).

    DO dDatoLoop = dDatoFra TO dDatoTil:
        FOR EACH kasse WHERE kasse.butik = tt_time1.butik NO-LOCK:
            FOR EACH NON_Sale_Spes WHERE non_sale_spes.butik = tt_time1.butik AND
                                         NON_Sale_Spes.kasse = kasse.kassenr  AND
                                         NON_Sale_Spes.dato  = dDatoloop NO-LOCK:
                ASSIGN tt_time1.non_salesum = tt_time1.non_salesum + NON_Sale_Spes.NON_SaleVerdi.
            END.
        END.
    END.
    tt_time1.VpK_salgsum  = round(tt_time1.var_salgsum / tt_time1.kundeant,2) NO-ERROR.
    tt_time1.TpK_salgsum  = round(tt_time1.tj_salgsum / tt_time1.kundeant,2) NO-ERROR.
    tt_time1.ApK_vareant  = round(tt_time1.vareant / tt_time1.kundeant,2) NO-ERROR.
    tt_time1.KpD_ant      = round(tt_time1.kundeant / int(entry(tt_time1.man,cAntDag)),2) NO-ERROR.
    tt_time1.VpD_salgsum  = round((tt_time1.var_salgsum + tt_time1.tj_salgsum) / int(entry(tt_time1.man,cAntDag)),2) NO-ERROR.
end.
for each tt_time2:
     tt_time2.TBprocent2    = round(tt_time2.tbsum2 / tt_time2.salgsum2 * 100,2).
     tt_time2.tj_TBprocent2 = round(tt_time2.tj_tbsum2 / tt_time2.tj_salgsum2 * 100,2).
end.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

