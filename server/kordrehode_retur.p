/* kordrehode_retur.p

-----------------------------------------------------------------------------------*/
DEF INPUT  PARAM icParam     AS CHAR NO-UNDO.
DEF INPUT  PARAM ihBuffer    AS HANDLE NO-UNDO.
DEF INPUT  PARAM icSessionId AS CHAR NO-UNDO.
DEF OUTPUT PARAM ocReturn    AS CHAR NO-UNDO.
DEF OUTPUT PARAM obOK        AS LOG NO-UNDO.

DEFINE VARIABLE cNettButikkType AS CHARACTER NO-UNDO.
DEFINE VARIABLE hQuery          AS HANDLE    NO-UNDO.
DEFINE VARIABLE ix              AS INT NO-UNDO.

/* Input parametre */                     
DEFINE VAR iButikkNr  AS INTEGER NO-UNDO.
DEFINE VAR iSelgernr  AS INTEGER NO-UNDO.
DEFINE VAR cTyp       AS CHAR    NO-UNDO.
DEFINE VAR cKOrdre_Id AS CHAR    NO-UNDO.
/* Input-Output */
DEFINE VAR lcTT       AS LONGCHAR  NO-UNDO.
/* Output parametre */
DEFINE VAR lcReturKoder     AS LONGCHAR  NO-UNDO.
DEFINE VAR bOk              AS LOG       NO-UNDO.
DEFINE VAR cKvittotext      AS CHARACTER NO-UNDO.
DEFINE VAR cEksterntOrdrenr AS CHARACTER NO-UNDO.
DEFINE VAR dReturKOrdre_Id  AS DECIMAL   NO-UNDO.
DEFINE VAR cReturn          AS CHARACTER NO-UNDO.
                   
/*{"kolinjer": [                             */
/*  {                                        */
/*    "artikkelnr": "9805725",               */
/*    "linjenr": 78008,                      */
/*    "ean": "7310840037177",                */
/*    "varetekst": "PIN STRIPE DOUBLE DUVET",*/
/*    "antall": 1,                           */
/*    "levfargkod": "44125\/NAVY",           */
/*    "storl": "200X220",                    */
/*    "kundpris": 1100.0,                    */
/*    "feilkode": 20,                        */
/*    "used": true                           */
/*  }                                        */
/*]}                                         */

DEFINE TEMP-TABLE KoLinjer
    FIELD artikkelnr AS CHARACTER 
    FIELD linjenr AS CHARACTER 
    FIELD ean AS CHARACTER 
    FIELD varetekst AS CHARACTER 
    FIELD antall AS INTEGER 
    FIELD levfargkod AS CHARACTER 
    FIELD storl AS CHARACTER 
    FIELD kundpris AS DECIMAL 
    FIELD feilkod AS INTEGER 
    FIELD used AS LOG FORMAT "true/false"
    . 

cNettButikkType = (DYNAMIC-FUNCTION("getFieldValues","SysPara",
                        "WHERE SysHId = 150 and SysGr = 1 and ParaNr = 20","Parameter1")).

/* Det kommer ingen temp-table inn, derfor opprettes den her. */
IF NUM-ENTRIES(icParam) > 2 THEN 
DO:
  IF ENTRY(2,icParam) = "ROWID" THEN
    DO ix = 3 TO NUM-ENTRIES(icParam):
      FIND KOrdreLinje WHERE ROWID(KOrdreLinje) = TO-ROWID(ENTRY(ix,icParam)) NO-LOCK NO-ERROR.
      IF AVAIL KOrdreLinje THEN DO:
        FIND KOrdreHode OF KOrdreLinje NO-LOCK.
        CREATE KoLinjer.  
        ASSIGN
            iButikkNr                = KOrdreHode.ButikkNr
            cKordre_Id               = STRING(KOrdreLinje.KOrdre_Id)
            KOLinjer.artikkelnr = KOrdreLinje.VareNr  
            KOLinjer.linjenr    = STRING(KOrdreLinje.KOrdreLinjeNr)
            KOLinjer.ean        = KOrdreLinje.Kode 
            KOLinjer.varetekst  = KOrdreLinje.Varetekst
            KOLinjer.antall     = INT(KOrdreLinje.Antall)
            KOLinjer.levfargkod = KOrdreLinje.LevFargKod 
            KOLinjer.storl      = KOrdreLinje.Storl
            KOLinjer.kundpris   = KOrdreLinje.Linjesum
            KOLinjer.feilkod    = 1 /* Flagger at linjer er returnert slik at den ikke kan returneres flere ganger. */
            KOLinjer.used       = FALSE 
            . 
      END.
    END.
END.

IF CAN-FIND(FIRST KoLinjer) THEN
DO:
    ASSIGN
        iSelgerNr  = 99
        cTyp       = "RETURNER"
        .

    TEMP-TABLE KoLinjer:WRITE-JSON("longchar",lcTT,TRUE,"UTF-8").
     
    RUN asReturPOSPhoenix.p(INPUT iButikkNr,
                            INPUT iSelgerNr,
                            INPUT cTyp,
                            INPUT cKOrdre_Id,
                            INPUT-OUTPUT lcTT,
                            OUTPUT lcReturkoder,
                            OUTPUT bOk,
                            OUTPUT cKvittoText,
                            OUTPUT cEksterntOrdreNr,
                            OUTPUT dReturKOrdre_Id,
                            OUTPUT cReturn
                           ).

/*MESSAGE 'Svar' SKIP                          */
/*    'lcReturKoder:' STRING(lcReturKoder) SKIP*/
/*    'bok:' bok SKIP                          */
/*    'cKvittotext:' cKvittotext SKIP          */
/*    'cEksterntOrdreNr:' cEksterntOrdreNr SKIP*/
/*    'dReturKOrdre_Id:' dReturKOrdre_Id SKIP  */
/*    'cReturn:' cReturn SKIP(1)               */
/*    'lcTT' STRING(lcTT)                      */
/*    VIEW-AS ALERT-BOX INFO BUTTONS OK.       */
/*                                             */
    
    ASSIGN 
        obOk     = TRUE
        ocReturn = ''
        .
END.
ELSE 
    ASSIGN 
        obOk     = FALSE
        ocReturn = 'Ingen ordrelinjer på angitt ordre som skal returneres.'
        .

