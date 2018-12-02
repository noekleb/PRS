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

DEFINE        VARIABLE  cKatalog    AS CHARACTER  NO-UNDO.
DEFINE        VARIABLE  dSistaDatum AS DATE       NO-UNDO. /* optimalt rapportdatum */
DEFINE        VARIABLE  cFilPrefix  AS CHARACTER  NO-UNDO.

DEFINE TEMP-TABLE TT_Butiker NO-UNDO
    FIELD butikknr AS INTEGER
    FIELD datum    AS DATE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-getLentusFilnavn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD getLentusFilnavn Procedure 
FUNCTION getLentusFilnavn RETURNS LOGICAL
  ( INPUT cDir AS CHARACTER,OUTPUT cLentusFil AS CHARACTER)  FORWARD.

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

/* Bygger butikslista i syspara */
RUN KontrollerSysPara.
/*  bygger temp-table */
RUN ByggButikTT.
/* OUTPUT TO "F:\home\lindbak\sendes\Lentus\butiker.txt". */
/* FOR EACH TT_Butiker:                                   */
/*     EXPORT tt_butiker.                                 */
/* END.                                                   */
/* OUTPUT CLOSE.                                          */
IF CAN-FIND(FIRST TT_Butiker) THEN
    RUN Rapport.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggButikTT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggButikTT Procedure 
PROCEDURE ByggButikTT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE ii        AS INTEGER    NO-UNDO.
DEFINE VARIABLE dTmpDato  AS DATE       NO-UNDO.
DEFINE VARIABLE d3112Fgar AS DATE       NO-UNDO.
DEFINE VARIABLE iSokDag AS INTEGER    NO-UNDO.
/* IF cButiker <> "" THEN DO ii = 1 TO NUM-ENTRIES(cButiker):         */
/*     CREATE TT_Butiker.                                             */
/*     ASSIGN TT_Butiker.butikknr = INT(ENTRY(ii,cButiker)) NO-ERROR. */
/*     IF ERROR-STATUS:ERROR THEN                                     */
/*         DELETE TT_Butiker.                                         */
/* END.                                                               */
/* ELSE DO:                                                           */
/*     FOR EACH Butiker NO-LOCK.                                      */
/*         CREATE TT_Butiker.                                         */
/*         ASSIGN TT_Butiker.butikknr = butiker.butik.                */
/*     END.                                                           */
/* END.                                                               */
/* dSistaDatum */
    FOR EACH Syspara WHERE SysPara.SysHId = 210 AND
                           SysPara.SysGr  = 255 NO-LOCK.
        /* hämta öppetschema och se om vi har dagar kvar att köra */
        FIND butiker WHERE butiker.butik = SysPara.ParaNr NO-LOCK NO-ERROR.
        IF NOT AVAIL butiker THEN
            NEXT.
        dTmpDato = DATE(SysPara.Parameter1).
        IF dTmpDato >= dSistaDatum THEN
            NEXT.
        DATO:
        DO dTmpDato = dTmpDato + 1 TO dSistaDatum:
            FIND Apnskjema WHERE ApnSkjema.butikknr = butiker.butik AND
                                 ApnSkjema.Ar = YEAR(dTmpDato) NO-LOCK NO-ERROR.
            IF NOT AVAIL ApnSkjema THEN
                LEAVE DATO.
            d3112Fgar = DATE(12,31,YEAR(dTmpDato) - 1).
            IF NUM-ENTRIES(ApnSkjema.openclosed) < dTmpDato - d3112Fgar THEN
                LEAVE DATO.
            iSokDag = dTmpDato - d3112Fgar. /* Rapportdag */
             IF LOOKUP("1",ApnSkjema.openclosed) <= iSokDag OR 
                    (LOOKUP("2",ApnSkjema.openclosed) > 0 AND LOOKUP("2",ApnSkjema.openclosed) <= iSokDag) THEN
                LEAVE DATO. /* När vi finner ikke behandlad dag för ett datum så lämnar vi */
/*             IF CAN-DO("1,2",ENTRY(dTmpDato - d3112Fgar,ApnSkjema.openclosed)) THEN */
/*                 LEAVE DATO.                                                        */
              
            CREATE TT_Butiker.
            ASSIGN TT_Butiker.butikknr = butiker.butik
                   TT_Butiker.datum    = dTmpdato.

        END.
    END.
    RELEASE TT_Butiker.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-KontrollerSyspara) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE KontrollerSyspara Procedure 
PROCEDURE KontrollerSyspara :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dFirstDate  AS DATE       NO-UNDO.
    DEFINE VARIABLE iMinusdagar AS INTEGER    NO-UNDO.
    DEFINE VARIABLE dTmpDato AS DATE       NO-UNDO.
    DEFINE VARIABLE cDato AS CHARACTER  NO-UNDO.
    {syspara.i 210 254 1 cKatalog}
    cKatalog = RIGHT-TRIM(cKatalog,"\").

    {syspara.i 210 254 2 iMinusdagar INT}
    ASSIGN dSistaDatum = TODAY - iMinusdagar.
    
    {syspara.i 210 254 3 dFirstDate DATE}
        dTmpDato = dFirstDate - 1.

    {syspara.i 210 254 6 cFilPrefix}

    FOR EACH butiker NO-LOCK WHERE CAN-FIND(FIRST Kasse WHERE Kasse.butik = butiker.butik AND Kasse.aktiv = TRUE):
        FIND Syspara WHERE SysPara.SysHId = 210 AND
                           SysPara.SysGr  = 255 AND
                           SysPara.ParaNr = butiker.butik NO-ERROR.
        IF NOT AVAIL SysPara AND butiker.NedlagtDato = ? THEN DO:
            CREATE SysPara.
            ASSIGN SysPara.SysHId = 210
                   SysPara.SysGr  = 255
                   SysPara.ParaNr = butiker.butik
                   SysPara.Beskrivelse = butiker.butnamn.
                   SysPara.Parameter1  = STRING(dTmpDato) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                DELETE syspara.
            END.
        END.
        ELSE IF AVAIL SysPara AND Butiker.NedlagtDato <> ? THEN DO:
            /* här tar vi bort om vi har kört fram till sista möjliga datum */
            IF Butiker.NedlagtDato <= DATE(SysPara.Parameter1) THEN
                DELETE syspara.
        END.
        ELSE IF AVAIL SysPara AND DATE(Syspara.parameter1) = ? THEN
            ASSIGN SysPara.Parameter1  = STRING(dTmpDato).
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
DEFINE VARIABLE dDato AS DATE       NO-UNDO.
DEFINE VARIABLE cSubtypeName AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSubTypeNr   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cKortNamn   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE IsUBTYPE AS INTEGER    NO-UNDO.
DEFINE VARIABLE cFilnamn AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cTmpFilename AS CHARACTER  NO-UNDO.

ASSIGN cSubtypeNr   = "1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25"
      cSubtypename = "PREEM,PREEM VISA,SÅIFA,TEPAR,HY/TEX NO,HY/TEX DK,SAAB/OPEL,VOLVO,NESTE,DKV,OK,UNO-X,BANKKORT,AMEX,DINERS,FINAX,UTA,BONUSKORT,CAMPING,,,,,,".

IF NOT getLentusFilnavn(cKatalog,OUTPUT cFilnamn) THEN
    RETURN "ERROR".
cTmpFilename = TRIM(cKatalog,"\") + "\" + "TMP_" +  
                                      STRING(YEAR(TODAY),"9999")  + 
                                      STRING(MONTH(TODAY),"99") + 
                                      STRING(DAY(TODAY),"99")   +
                                      REPLACE(STRING(TIME,"HH:MM"),":","") +
                                     ".txt".

OUTPUT TO VALUE(cTmpFilename).
PUT UNFORMATTED "Butik" ";"
                "Kassenr" ";"
                "Datum" ";"
                "Kl" ";"
                "Numtid" ";"
                "Kvittonr" ";"
                "Ttid" ";"
                "Transbeskr" ";"
                "Kort" ";"
                "Artikkelnr" ";"
                "Ean" ";"
                "Kvittotext" ";"
                "Hgr" ";"
                "Antal" ";"
                "Sum" ";"
                "Mva" ";"
                "Rab" SKIP.

FOR EACH TT_Butiker:
/*     DO dDato = dDatoFra TO dDatoTil: */
    DO dDato = TT_Butiker.datum TO TT_Butiker.datum:

        FOR EACH kasse WHERE kasse.butikknr = TT_Butiker.butikknr AND kasse.kassenr < 11 NO-LOCK:
            FOR EACH bonghode WHERE bonghode.butikknr = TT_Butiker.butikknr AND
                                            bonghode.gruppenr = 1 AND
                                            bonghode.kassenr  = kasse.kassenr AND
                                            bonghode.dato     = dDato NO-LOCK.
                IF NOT CAN-FIND(FIRST Bonglinje WHERE bonglinje.b_id = bonghode.b_id AND
                                                      bonglinje.bongtekst BEGINS "KUPONG" AND
                                                      bonglinje.makulert = FALSE) THEN
                    NEXT.
                FOR EACH bonglinje WHERE bonglinje.b_id = bonghode.b_id NO-LOCK.
                    IF bonglinje.makulert = TRUE THEN
                        NEXT.
                    cKortNamn = "".
                    FIND transtype OF bonglinje NO-LOCK NO-ERROR.
                    IF bonglinje.ttid = 52 OR bonglinje.ttid = 58 THEN DO:
                        iSubtype = bonglinje.antall.
                        IF CAN-DO(cSubtypeNr,STRING(iSubType)) THEN
                            ASSIGN cKortnamn = ENTRY(iSubType,cSubTypename).
                    END.
                    PUT UNFORMATTED TT_Butiker.butikknr ";"
                                    kasse.kassenr ";"
                                    bonghode.dato ";"
                                    STRING(bonghode.tid,"HH:MM:SS") ";"
                                    bonghode.tid ";"
                                    bonghode.bongnr ";"
                                    bonglinje.ttid ";"
                                    (IF AVAIL transtype THEN TransType.Beskrivelse ELSE "") ";"
                                    cKortnamn ";"
                                    TRIM(bonglinje.artikkelnr) ";"
                                    TRIM(bonglinje.strekkode) ";"
                                    REPLACE(Bonglinje.bongtekst,";"," ") ";"
                                    (IF bonglinje.hovedgr > 0 THEN string(bonglinje.hovedgr) ELSE "") ";"
                                    (IF bonglinje.ttid < 13 THEN STRING(bonglinje.antall) ELSE "") ";"
                                    bonglinje.linjesum ";"
                                    bonglinje.mvakr ";"
                                    bonglinje.linjerab SKIP
                                     .
                END.
            END.
        END.
        FIND SysPara WHERE SysPara.SysHId = 210 AND
                            SysPara.SysGr  = 255 AND
                            SysPara.ParaNr = TT_butiker.butik.
        ASSIGN SysPara.Parameter1 = STRING(dDato).

    END.
END.
OUTPUT CLOSE.
IF SEARCH(cTmpFilename) <> ? THEN
    OS-RENAME VALUE(cTmpFilename) VALUE(cFilNamn).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-getLentusFilnavn) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION getLentusFilnavn Procedure 
FUNCTION getLentusFilnavn RETURNS LOGICAL
  ( INPUT cDir AS CHARACTER,OUTPUT cLentusFil AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cSeqFil AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE iSeqNr  AS INTEGER    NO-UNDO.
    DEFINE VARIABLE cTxt AS CHARACTER  NO-UNDO.

/* till.från.uppsamlingsperiod.innehåll.sekvensnummer.löpnummer.tidstämpel.md5.filtyp.komprimering */
/* Lentus.Preem.Yd-20070301.trans-1.0001.1.200703020358.fixed                                      */

    cDir = TRIM(cDir,"\").
    ASSIGN cSeqFil = cDir + "\" + "seqnr.txt".
    IF SEARCH(cSeqFil) = ? THEN DO:
        OS-COMMAND SILENT VALUE("md " + cDir).
        iSeqNr = 1.
        OUTPUT TO VALUE(cSeqFil).
        OUTPUT CLOSE.
    END.
    ELSE DO:
        INPUT FROM value(cSeqFil) NO-ECHO.
        IMPORT UNFORMATTED cTxt.
        iSeqNr = INT(cTxt) + 1.
        INPUT CLOSE.
    END.
    IF SEARCH(cSeqFil) = ? THEN DO:
        RETURN FALSE.
    END.
    ASSIGN cLentusFil = cDir + "\" + "Lentus.Preem.Yd-"    + 
                                      STRING(YEAR(dSistaDatum),"9999")  + 
                                      STRING(MONTH(dSistaDatum),"99") + 
                                      STRING(DAY(dSistaDatum),"99")   +
                                      ".kupongkvitto-1." +
                                      STRING(iSeqNr,"9999") +
                                      ".1." +
                                      STRING(YEAR(TODAY),"9999")  + 
                                      STRING(MONTH(TODAY),"99") + 
                                      STRING(DAY(TODAY),"99")   +
                                      REPLACE(STRING(TIME,"HH:MM"),":","") +
                                     ".fixed".
    OUTPUT TO VALUE(cSeqFil).
    PUT UNFORMATTED iSeqNr SKIP.
    OUTPUT CLOSE.

  RETURN TRUE.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

