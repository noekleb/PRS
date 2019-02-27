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

DEFINE INPUT  PARAMETER iButik     LIKE Butiker.Butik    NO-UNDO.
DEFINE INPUT  PARAMETER dDato      AS DATE               NO-UNDO.
DEFINE INPUT  PARAMETER cKatalog   AS CHARACTER          NO-UNDO.
DEFINE INPUT  PARAMETER cExtraPara AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cFilNavn    AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cNumFormat  AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cDateFormat AS CHARACTER  NO-UNDO.


DEFINE TEMP-TABLE TT_Xlnt
    FIELD Artikkelnr LIKE ArtBas.Artikkelnr
    FIELD Vg         LIKE VarGr.Vg
    FIELD our_no     AS DECIMAL DECIMALS 0
    FIELD Dato       AS DATE FORMAT "9999-99-99"
    FIELD UkeNo      AS INTEGER FORMAT ">9"
    FIELD SumAntall  AS DECIMAL FORMAT "->>>,>>9.99"
    FIELD SumSalg    AS DECIMAL FORMAT "->>>,>>>.99"
    FIELD SumInpris  AS DECIMAL FORMAT "->>>,>>>.99"
    FIELD SumRab     AS DECIMAL FORMAT "->>>,>>>.99"
    FIELD SumMva     AS DECIMAL FORMAT "->>>,>>>.99"
    FIELD SumKunder  AS INTE FORMAT "->>>9"
    FIELD StoreSellU AS DECI FORMAT "->>>,>>>.99"
    INDEX Artikkelnr Artikkelnr.

DEFINE TEMP-TABLE TT_AntKunder
    FIELD our_no     AS DECIMAL DECIMALS 0
    FIELD Dato       AS DATE FORMAT "9999-99-99"
    FIELD UkeNo      AS INTEGER FORMAT ">9"
    FIELD SumKunder  AS INTE FORMAT "->>>9".

DEFINE TEMP-TABLE tmpTT_Xlnt LIKE TT_Xlnt.

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

IF cKatalog = "" THEN
    ASSIGN cKatalog = "\home\lindbak\sendes".
ASSIGN cKatalog    = RIGHT-TRIM(cKatalog,"\")
       cKatalog    = cKatalog + "\" + STRING(iButik) + "\"
       cNumFormat  = SESSION:NUMERIC-FORMAT
       cDateFormat = SESSION:DATE-FORMAT.
RUN SkapaDir IN THIS-PROCEDURE (cKatalog).
RUN SummeraSalg IN THIS-PROCEDURE.
RUN ExporterToXlnt.
ASSIGN SESSION:NUMERIC-FORMAT = cNumFormat 
       SESSION:DATE-FORMAT    = cDateFormat.

RETURN cFilnavn. /*  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExporterToXlnt) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExporterToXlnt Procedure 
PROCEDURE ExporterToXlnt :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFilnavnAntKunder AS CHARACTER  NO-UNDO.
    ASSIGN cFilnavn = 
        cKatalog + String(iButik,"999999") + "_" + STRING(YEAR(dDato),"9999") + STRING(MONTH(dDato),"99") + STRING(DAY(dDato),"99") + ".txt".
    /* detta återställs i main-blocket */
    ASSIGN SESSION:NUMERIC-FORMAT = "American"
       SESSION:DATE-FORMAT        = "ymd".
    IF NOT CAN-FIND(FIRST TT_Xlnt) THEN
        RETURN.
/*     OUTPUT TO VALUE(cKatalog + String(iButik,"999999") + "_" + STRING(YEAR(dDato),"9999") + STRING(MONTH(dDato),"99") + STRING(DAY(dDato),"99") + ".txt"). */
    OUTPUT TO VALUE(cFilnavn).
    FOR EACH TT_Xlnt:
        PUT UNFORMATTED TRIM(STRING(TT_Xlnt.Artikkelnr,">>>>>>>>>>>>9")) ";"
            TRIM(STRING(TT_Xlnt.Vg))         ";"
            TRIM(STRING(TT_Xlnt.our_no))     ";"
            TRIM(STRING(TT_Xlnt.Dato,"9999-99-99")) ";"
            TRIM(STRING(TT_Xlnt.UkeNo))      ";"
            TRIM(STRING(TT_Xlnt.SumAntall))  ";"
            TRIM(STRING(TT_Xlnt.SumSalg - TT_Xlnt.SumRab,"->>>,>>>.99"))    ";"
            TRIM(STRING(TT_Xlnt.SumInpris))  ";"
            TRIM(STRING(TT_Xlnt.SumRab))     ";"
            TRIM(STRING(TT_Xlnt.SumMva))     ";"
            TRIM(STRING(TT_Xlnt.SumKunder))  ";"
            TRIM(STRING(TT_Xlnt.StoreSellU)) SKIP.
/*               TRIM(STRING(TT_Xlnt.Vg,">>>>>9"))         ";"       */
/*               TRIM(STRING(TT_Xlnt.our_no))     ";"                */
/*               TRIM(STRING(TT_Xlnt.Dato,"9999-99-99"))       ";"   */
/*               TRIM(STRING(TT_Xlnt.UkeNo,">9"))      ";"           */
/*               TRIM(STRING(TT_Xlnt.SumAntall,">>>,>>9.99"))  ";"   */
/*               TRIM(STRING(TT_Xlnt.SumSalg,">>>,>>9.99"))    ";"   */
/*               TRIM(STRING(TT_Xlnt.SumInpris,">>>,>>9.99"))  ";"   */
/*               TRIM(STRING(TT_Xlnt.SumRab,">>>,>>9.99"))     ";"   */
/*               TRIM(STRING(TT_Xlnt.SumMva,">>>,>>9.99"))     ";"   */
/*               TRIM(STRING(TT_Xlnt.SumKunder,">>>,>>9"))  ";"      */
/*               TRIM(STRING(TT_Xlnt.StoreSellU,">>>,>>9.99")) SKIP. */
    END.
    OUTPUT CLOSE.
    ASSIGN cFilnavnAntKunder = 
        cKatalog + "K" + String(iButik,"999999") + "_" + STRING(YEAR(dDato),"9999") + STRING(MONTH(dDato),"99") + STRING(DAY(dDato),"99") + ".txt".
    OUTPUT TO VALUE(cFilnavnAntKunder).
    FOR EACH TT_AntKunder:
        PUT UNFORMATTED TRIM(STRING(TT_AntKunder.our_no))     ";"
                        TRIM(STRING(TT_AntKunder.Dato,"9999-99-99")) ";"
                        TRIM(STRING(TT_AntKunder.UkeNo))      ";"
                        TRIM(STRING(TT_AntKunder.SumKunder)) SKIP.
    END.
    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SkapaDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SkapaDir Procedure 
PROCEDURE SkapaDir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER cKatalog AS CHARACTER  NO-UNDO.
  DEFINE        VARIABLE  iCount   AS INTEGER    NO-UNDO.
  DEFINE         VARIABLE cDir     AS CHARACTER  NO-UNDO.
  ASSIGN cDir = ENTRY(1,cKatalog,"\") + "\".
  DO iCount = 2 TO NUM-ENTRIES(cKatalog,"\") - 1:
      cDir = cDir + ENTRY(iCount,cKatalog,"\") + "\".
      OS-CREATE-DIR VALUE(cDir).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-SummeraSalg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SummeraSalg Procedure 
PROCEDURE SummeraSalg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iWeekNum AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iCount   AS INTEGER    NO-UNDO.
    DEFINE VARIABLE iKoeff   AS INTEGER    NO-UNDO.
    RUN weeknum.p (INPUT dDato, OUTPUT iWeekNum).
    ASSIGN iWeekNum = INT(SUBSTR(STRING(iWeekNum),5)).
    CREATE TT_AntKunder.
    ASSIGN TT_AntKunder.our_no = iButik
           TT_AntKunder.Dato   = dDato  
           TT_AntKunder.UkeNo  = iWeekNum.
    DO iCount = 1 TO 10:
        FOR EACH BongHode WHERE BongHode.Butik   = iButik AND
                                BongHode.Gruppe  = 1      AND
                                BongHode.KasseNr = iCount AND
                                BongHode.Dato    = dDato NO-LOCK:
            EMPTY TEMP-TABLE tmpTT_Xlnt.
            ASSIGN TT_AntKunder.SumKunder = TT_AntKunder.SumKunder + 1.
            FOR EACH BongLinje WHERE BongLinje.b_id = BongHode.b_id AND
                 CAN-DO("001,003,010,012",STRING(BongLinje.ttid,"999")).
                ASSIGN iKoeff = IF NOT BongLinje.Antall < 0 THEN 1 ELSE -1.
                FIND strekkode WHERE strekkode.kode = TRIM(BongLinje.strekkode) NO-LOCK NO-ERROR.
                IF NOT AVAIL Strekkode THEN
                    NEXT.
                FIND tmpTT_Xlnt WHERE tmpTT_Xlnt.Artikkelnr = strekkode.ArtikkelNr NO-ERROR.
/*                 FIND tmpTT_Xlnt WHERE tmpTT_Xlnt.Artikkelnr = DECI(BongLinje.ArtikkelNr) NO-ERROR. */
                IF NOT AVAIL tmpTT_Xlnt THEN DO:
                    CREATE tmpTT_Xlnt.
                    ASSIGN tmpTT_Xlnt.ArtikkelNr = Strekkode.artikkelnr /* DECI(BongLinje.ArtikkelNr) */
                           tmpTT_Xlnt.Vg         = BongLinje.VareGr
                           tmpTT_Xlnt.our_no     = BongLinje.Butik
                           tmpTT_Xlnt.Dato       = BongLinje.Dato
                           tmpTT_Xlnt.UkeNo      = iWeekNum
                           tmpTT_Xlnt.StoreSellU = 1.
                END.
                ASSIGN tmpTT_Xlnt.SumAntall  = tmpTT_Xlnt.SumAntall + BongLinje.Antall
                       tmpTT_Xlnt.SumSalg    = tmpTT_Xlnt.SumSalg   + (iKoeff * BongLinje.LinjeSum)
                       tmpTT_Xlnt.SumInpris  = tmpTT_Xlnt.SumInpris + BongLinje.VVareKost
/*                        tmpTT_Xlnt.SumInpris  = tmpTT_Xlnt.SumInpris + (BongLinje.Antall * BongLinje.VVareKost) */
                       tmpTT_Xlnt.SumRab     = tmpTT_Xlnt.SumRab    + (iKoeff * BongLinje.LinjeRab + iKoeff * BongLinje.SubtotalRab)
                       tmpTT_Xlnt.SumMva     = tmpTT_Xlnt.SumMva    + (iKoeff * MvaKr)
    /*                    tmpTT_Xlnt.SumKunder  = tmpTT_Xlnt.SumKunder   */
    /*                    tmpTT_Xlnt.StoreSellU = tmpTT_Xlnt.StoreSellU  */
                           .

            END.
            FOR EACH tmpTT_Xlnt:
                FIND TT_Xlnt WHERE TT_Xlnt.ArtikkelNr = tmpTT_Xlnt.ArtikkelNr NO-ERROR.
                IF NOT AVAIL TT_Xlnt THEN DO:
                    BUFFER-COPY tmpTT_Xlnt TO TT_Xlnt.
                    ASSIGN TT_Xlnt.SumKunder = 1.
                END.
                ELSE DO:         
                    ASSIGN TT_Xlnt.SumAntall  = TT_Xlnt.SumAntall + tmpTT_Xlnt.SumAntall 
                           TT_Xlnt.SumSalg    = TT_Xlnt.SumSalg   + tmpTT_Xlnt.SumSalg   
                           TT_Xlnt.SumInpris  = TT_Xlnt.SumInpris + tmpTT_Xlnt.SumInpris 
                           TT_Xlnt.SumRab     = TT_Xlnt.SumRab    + tmpTT_Xlnt.SumRab    
                           TT_Xlnt.SumMva     = TT_Xlnt.SumMva    + tmpTT_Xlnt.SumMva
                           TT_Xlnt.SumKunder  = TT_Xlnt.SumKunder + 1
                           .
                END.
            END.
        END.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

