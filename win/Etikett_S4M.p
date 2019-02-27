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

DEFINE INPUT  PARAMETER cMPmodell AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER cPRINTER  AS CHARACTER   NO-UNDO.
DEFINE INPUT  PARAMETER lTermKlient AS LOGICAL     NO-UNDO.

/* Local Variable Definitions ---                                       */
DEF VAR wTekst1      AS CHAR NO-UNDO.
DEF VAR wTekst2      AS CHAR NO-UNDO.
def var wCl          as int  no-undo.
def var wSeqNr       as int  no-undo.
def var wLayout      as char no-undo.
def var wBatch       as char no-undo.
def var wEtikett_Fil as char INIT "etikett.dat" no-undo.
def var wFirma       as char no-undo.
def var Linje        as char extent 50.
def var BLinje       as char extent 50.
def var TStorlek     as char format "x(4)".
def var iTeller       as int  no-undo.
DEF VAR iEtikettTyp  AS INTE NO-UNDO.



{etikettlogg.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-ChrConv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ChrConv Procedure 
FUNCTION ChrConv RETURNS CHARACTER
  ( INPUT BongStr AS CHARACTER )  FORWARD.

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
/* RUN Initiering. */
/* MESSAGE                                */
/* "cMPmodell"   cMPmodell  SKIP          */
/* "cPRINTER"    cPRINTER   SKIP          */
/* "lTermKlient" lTermKlient              */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */


RUN VALUE("Utskrift" + cMPmodell).

EMPTY TEMP-TABLE etikettlogg.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Initiering) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Initiering Procedure 
PROCEDURE Initiering :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE ii AS INTEGER     NO-UNDO.
    IF cMPmodell = "MARKPOINT_1" THEN DO:
        ASSIGN 
        linje[1] = '!C' + CHR(13) + '!C' + CHR(13) + '!Y24 50' + CHR(13) + 
                   '!Y100 0' + CHR(13) + '!Y35 10' + CHR(13) + '!Y42 0' + CHR(13)
        linje[2] = '!F C E 680 10 L 63 2 1 "%1V"'       + CHR(13) /* Interleave */
        linje[3] = '!Y42 1'                             + CHR(13)
        linje[4] = '!F C N 150 800 L 56 2 32 "%2V"'      + CHR(13) /* EAN */
        linje[5] = '!Y162 0'                            + CHR(13)
        linje[6] = '!F S N 230 900 C 11 8 94021 "%3V"'   + CHR(13) /* "001 / 0001 stlk  99,0" */
        linje[7] = '!F S W 780 90 L 10 10 94021 "%4V"' + CHR(13) /* "2021" PLU */
        linje[8] = '!F S N 60 970 R 19 19 94021 "%5V"' + CHR(13) /* "9999" KR */
        linje[9] = '!F S N 40 990 C 11 11 94021 "%6V"' + CHR(13). /* "50" Ören */
        wlayout = "".
        do ii = 1 to 9:
            wlayout = wlayout + linje[ii].
        end.
    END.
    ELSE IF cMPmodell = "MARKPOINT_2" THEN DO:
        ASSIGN 
        linje[1] = '!C' + CHR(13) + '!C' + CHR(13) + '!Y24 50' + CHR(13) + 
                   '!Y100 0' + CHR(13) + '!Y35 10' + CHR(13) + '!Y42 0' + CHR(13)
        linje[2] = '!F C E 680 10 L 63 2 1 "%1V"'       + CHR(13) /* Interleave */
        linje[3] = '!Y42 1'                             + CHR(13)
        linje[4] = '!F C N 150 800 L 56 2 32 "%2V"'      + CHR(13) /* EAN */
        linje[5] = '!Y162 0'                            + CHR(13)
        linje[6] = '!F S N 230 900 C 11 8 94021 "%3V"'   + CHR(13) /* "001 / 0001 stlk  99,0" */
        linje[7] = '!F S W 780 90 L 10 10 94021 "%4V"' + CHR(13) /* "2021" PLU */
        linje[8] = '!F S N 60 970 R 19 19 94021 "%5V"' + CHR(13) /* "9999" KR */
        linje[9] = '!F S N 40 990 C 11 11 94021 "%6V"' + CHR(13). /* "50" Ören */
        wlayout = "".
        do ii = 1 to 9:
            wlayout = wlayout + linje[ii].
        end.
    END.
    ELSE IF cMPmodell = "MARKPOINT_0" THEN DO: /* standard */
        ASSIGN 
            linje[1] = '!C' + CHR(13) +
                        '!Y24 50' + CHR(13) + '!Y100 0' + CHR(13) + '!Y35 10' + CHR(13) + '!Y42 0' + CHR(13)
            linje[2] = '!F C N 440 700 L 78 2 2 "%1V"' + CHR(13) /* interleave */
            linje[3] = '!Y162 0' + CHR(13)
            linje[4] = '!F S S 290 830 C 22 9 94021 "%2V"' + CHR(13) /* " 999/9999 stlk 35.5" */
            linje[5] = '!F S S 460 735 R 20 19 94021 "%3V"' + CHR(13) /* "99999" */
            linje[6] = '!F S S 480 710 C 11 11 94021 "%4V"' + CHR(13) /* "50" */
            linje[7] = '!Y42 0' + CHR(13)
            linje[8] = '!F C N 160 700 L 78 2 2 "%1V"' + CHR(13) /* interleave "001000100010" */
            linje[9] = '!F S S 10 830 C 22 9 94021 "%2V"' + CHR(13) /* " 999/9999 stlk 35.5" */
            linje[10] = '!F S S 180 735 R 20 19 94021 "%3V"' + CHR(13)   /* "99999" KR */
            linje[11] = '!F S S 200 710 C 11 11 94021 "%4V"' + CHR(13) /* "50" Ören */
            linje[12] = '!F S S 180 990 C 11 11 94021 "KR:"' + CHR(13)
            linje[13] = '!F S S 460 990 C 11 11 94021 "KR:"' + CHR(13)
            linje[14] = '!F S E 990 290 L 10 7 94021 "%1C"' + CHR(13) /* counter */
            linje[15] = '!F S E 990 15 L 10 7 94021 "%1C"' + CHR(13). /* counter */

/*         linje[1] = '!C' + CHR(13) +                                                                      */
/*                     '!Y24 50' + CHR(13) + '!Y100 0' + CHR(13) + '!Y35 10' + CHR(13) + '!Y42 0' + CHR(13) */
/*         linje[2] = '!F C S 70 1000 L 78 2 2 "%1V"' + CHR(13) /* interleave */                            */
/*         linje[3] = '!Y162 0' + CHR(13)                                                                   */
/*         linje[4] = '!F S N 220 880 C 19 9 92504 "%2V"' + CHR(13) /* " 999/9999 stlk 35.5" */             */
/*         linje[5] = '!F S N 50 970 R 20 24 92504 "%3V"' + CHR(13) /* "99999" */                           */
/*         linje[6] = '!F S N 30 970 L 11 15 92504 "%4V"' + CHR(13) /* "50" */                              */
/*         linje[7] = '!Y42 0' + CHR(13)                                                                    */
/*         linje[8] = '!F C S 350 1000 L 78 2 2 "%1V"' + CHR(13) /* interleave "001000100010" */            */
/*         linje[9] = '!F S N 500 880 C 19 9 92504 "%2V"' + CHR(13) /* " 999/9999 stlk 35.5" */             */
/*         linje[10] = '!F S N 330 970 R 19 19 92504 "%3V"' + CHR(13)   /* "99999" KR */                    */
/*         linje[11] = '!F S N 310 970 L 11 15 92504 "%4V"' + CHR(13) /* "50" Ören */                       */
/*         linje[12] = '!F S N 320 710 C 11 11 92504 "KR:"' + CHR(13)                                       */
/*         linje[13] = '!F S N 50 710 C 11 11 92504 "KR:"' + CHR(13)                                        */
/*         linje[14] = '!F S W 710 220 L 10 7 94021 "%1C"' + CHR(13) /* counter */                          */
/*         linje[15] = '!F S W 710 495 L 10 7 94021 "%1C"' + CHR(13). /* counter */                         */
        do ii = 1 to 15:
            wlayout = wlayout + linje[ii].
        end.
    END.
    ELSE IF cMPmodell = "MARKPOINT_L" THEN DO: /* liten */
        ASSIGN 
        linje[1] = '!C' + CHR(13) +
                    '!Y24 20' + CHR(13) + '!Y100 0' + CHR(13) + '!Y35 10' + CHR(13) + '!Y42 0' + CHR(13)
        linje[2] = '!F C N 340 700 L 78 2 2 "%1V"' + CHR(13) /* interleave */
        linje[3] = '!Y162 0' + CHR(13)
        linje[4] = '!F S S 190 830 C 22 9 94021 "%2V"' + CHR(13) /* " 999/9999 stlk 35.5" */
        linje[5] = '!F S S 360 730 R 20 19 94021 "%3V"' + CHR(13) /* "99999" */
        linje[6] = '!F S S 380 710 C 11 11 94021 "%4V"' + CHR(13) /* "50" */
        linje[7] = '!Y42 0' + CHR(13)
        linje[8] = '!F C N 100 700 L 53 2 2 "%1V"' + CHR(13) /* interleave "001000100010" */
        linje[9] = '!F S S 10 830 C 14 11 94029 "%2V"' + CHR(13) /* " 999/9999 stlk 35.5" */
        linje[10] = '!F S S 110 730 R 13 19 94029 "%3V"' + CHR(13)   /* "99999" KR */
        linje[11] = '!F S S 120 710 C 9 11 94029 "%4V"' + CHR(13) /* "50" Ören */
        linje[12] = '!F S S 110 990 C 10 11 94021 "KR:"' + CHR(13)
        linje[13] = '!F S S 360 990 C 11 11 94021 "KR:"' + CHR(13)
        linje[14] = '!F S E 990 190 L 10 7 94021 "%1C"' + CHR(13) /* counter */
        linje[15] = '!F S E 990 5 L 10 5 94021 "%1C"' + CHR(13). /* counter */
        

/*         ASSIGN                                                                                           */
/*         linje[1] = '!C' + CHR(13) +                                                                      */
/*                     '!Y24 50' + CHR(13) + '!Y100 0' + CHR(13) + '!Y35 10' + CHR(13) + '!Y42 0' + CHR(13) */
/*         linje[2] = '!F C S 70 1000 L 78 2 2 "%1V"' + CHR(13) /* interleave */                            */
/*         linje[3] = '!Y162 0' + CHR(13)                                                                   */
/*         linje[4] = '!F S N 220 880 C 19 9 92504 "%2V"' + CHR(13) /* " 999/9999 stlk 35.5" */             */
/*         linje[5] = '!F S N 50 970 R 20 24 92504 "%3V"' + CHR(13) /* "99999" */                           */
/*         linje[6] = '!F S N 30 970 L 11 15 92504 "%4V"' + CHR(13) /* "50" */                              */
/*         linje[7] = '!Y42 0' + CHR(13)                                                                    */
/*         linje[8] = '!F C S 310 1000 L 53 2 2 "%1V"' + CHR(13) /* interleave "001000100010" */            */
/*         linje[9] = '!F S N 400 880 C 14 8 92504 "%2V"' + CHR(13) /* " 999/9999 stlk 35.5" */             */
/*         linje[10] = '!F S N 300 970 R 13 19 92504 "%3V"' + CHR(13)   /* "99999" KR */                    */
/*         linje[11] = '!F S N 290 970 L 9 11 92504 "%4V"' + CHR(13) /* "50" Ören */                        */
/*         linje[12] = '!F S N 300 710 C 10 11 92504 "KR:"' + CHR(13)                                       */
/*         linje[13] = '!F S N 50 710 C 11 11 92504 "KR:"' + CHR(13)                                        */
/*         linje[14] = '!F S W 710 220 L 10 7 94021 "%1C"' + CHR(13) /* counter */                          */
/*         linje[15] = '!F S W 710 410 L 10 5 94021 "%1C"' + CHR(13). /* counter */                         */
        do ii = 1 to 15:
            wlayout = wlayout + linje[ii].
        end.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ORG_UtskriftS4M) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ORG_UtskriftS4M Procedure 
PROCEDURE ORG_UtskriftS4M :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cInterleave AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEan       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cTxt       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cPlu       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cKr        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cOren      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cAntal     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE ii         AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cStorl AS CHARACTER   NO-UNDO.
/*     OUTPUT TO "CLIPBOARD".                       */
/*                                                  */
/*     FOR EACH etikettlogg:                        */
/*         DISP etikettlogg WITH 1 COL SIDE-LABELS. */
/*     END.                                         */
/*     OUTPUT CLOSE.                                */
    IF NOT lTermklient THEN
        OUTPUT TO PRINTER VALUE (cPRINTER).
    ELSE
        OUTPUT TO VALUE (cPRINTER).
/*       PUT CONTROL wLayout. */
OUTPUT TO "CLIPBOARD".
FOR EACH etikettlogg:
    EXPORT etikettlogg.
END.
OUTPUT CLOSE.
RETURN.
  /*   output close. */
      for each EtikettLogg
         break by EtikettLogg.butik
               BY EtikettLogg.vg
               by EtikettLogg.lopnr
               by EtikettLogg.Storl:
          TStorlek = "".
          do ii = 1 to length(EtikettLogg.Storl):
              if substring(EtikettLogg.Storl,ii,1) = " " then
                TStorlek = TStorlek + "0". /* Space */
              else if substring(EtikettLogg.Storl,ii,1) = "." then
                NEXT. /* Punktum strippes bort. */
              else if (substring(EtikettLogg.Storl,ii,1) < "0" or
                       substring(EtikettLogg.Storl,ii,1) > "9") then
                TStorlek = TStorlek + "0". /* Alfanumeriske st|rrelser. */
              else
                TStorlek = TStorlek + substring(EtikettLogg.Storl,ii,1). /* Tar vare p} verdien */
          end.
          ASSIGN TStorlek = TRIM(TStorlek) /* Trimmer bort ledende space og gjør lengden = 4. */
                 TStorlek = (if LENGTH(TStorlek) = 2 then TStorlek + "0" else TStorlek)
                 TStorlek = "0" + TStorlek.
       ASSIGN cInterleave = STRING(EtikettLogg.vg,"999") + STRING(EtikettLogg.lopnr,"9999") + "0" + TStorlek + CHR(13)
/*               cEan       = (IF EtikettLogg.Ean = "0" THEN "" ELSE EtikettLogg.Ean) + CHR(13) */
              cEan       = "" + CHR(13)
              cStorl = LEFT-TRIM(Tstorlek,"0")
              cStorl = SUBSTR(cStorl,1,LENGTH(cStorl) - 1) + "." + SUBSTR(cStorl,LENGTH(cStorl))
              cTxt       = STRING(EtikettLogg.vg) + "-" + STRING(EtikettLogg.lopnr) + "/" + cStorl + CHR(13)
/*               cPlu       = STRING(EtikettLogg.plunr) + CHR(13) */
              cPlu       = "" + CHR(13)
              cKr        = ENTRY(1,STRING(EtikettLogg.pris,"zzzz9.99")) + CHR(13)
              cOren      = ENTRY(2,STRING(EtikettLogg.pris,">>>9.99")) + CHR(13)
              cAntal     = '!P' + STRING(EtikettLogg.Ant) + CHR(13).
       iTeller = iTeller + EtikettLogg.Ant.

       PUT UNFORMATTED
       "^XA" SKIP
       "^FO1,5^BY2" SKIP
       "^B2N,80,N,N,N" SKIP
       "^FD" cInterleave "^FS" SKIP
       "^FO1,90" SKIP
       "^ADN,36,10" SKIP
       "^FD" cTxt "^FS" SKIP
       "^FO1,130" SKIP
       "^ADN,36,20" SKIP
       "^FDKr: " cKr "." cOren "^FS" SKIP
       "^PQ" STRING(EtikettLogg.Ant * 2) SKIP
       "^MCY" SKIP
       "^XZ".




       put control cInterleave.
       put control cTxt.
       put control cKr.
       put control cOren.
/*        PUT CONTROL "1". */
/*        put control cInterleave. */
/*        put control cTxt.       */
/*        PUT CONTROL "1".        */
/*        put control cKr.        */
/*        put control cOren.      */
       PUT CONTROL cAntal.
    end.
    OUTPUT CLOSE.
    DO:
        OUTPUT to teller no-echo.
        IF iTeller > 9999 THEN
            iTeller = iTeller - 9999.
        EXPORT iTeller.
        OUTPUT close.
    END.
    EMPTY TEMP-TABLE etikettlogg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UtskriftDM3_L) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtskriftDM3_L Procedure 
PROCEDURE UtskriftDM3_L :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cInterleave AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEan       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cTxt       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cPlu       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cKr        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cOren      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cTeller     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER     NO-UNDO.
    DEFINE VARIABLE ii         AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cStartSlut AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cBestTxt AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cButikTxt AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cStorl AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cGetPr AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cSasong AS CHARACTER   NO-UNDO.
    cPRINTER = "*" + cPRINTER + "*".
    cGetPr = SESSION:GET-PRINTERS().
    DO ii = 1 TO NUM-ENTRIES(cGetPr):
        IF ENTRY(ii,cGetPr) MATCHES cPRINTER THEN DO:
            cPRINTER = ENTRY(ii,cGetPr).
        END.
    END.
    /*     OUTPUT TO "CLIPBOARD".                       */
/*                                                  */
/*     FOR EACH etikettlogg:                        */
/*         DISP etikettlogg WITH 1 COL SIDE-LABELS. */
/*     END.                                         */
/*     OUTPUT CLOSE.                                */
/*     OUTPUT TO "CLIPBOARD".  */
/*     FOR EACH etikettlogg:   */
/*         EXPORT etikettlogg. */
/*     END.                    */
/*     OUTPUT CLOSE.           */
/*     RETURN.                 */
/*                             */
    IF SEARCH("teller") <> ? THEN
    DO:
        INPUT from teller no-echo.
        IMPORT UNFORMATTED cTeller.
        ASSIGN iTeller = INT(cTeller).
/*             set iTeller. */
        INPUT close.
        IF (iTeller = 0 OR iTeller > 9999) THEN iTeller = 1.
    END.
    ELSE iTeller = 1.

    IF NOT lTermklient THEN
        OUTPUT TO PRINTER VALUE (cPRINTER).
    ELSE
        OUTPUT TO VALUE (cPRINTER).
/*       PUT CONTROL wLayout. */
  /*   output close. */
    for each EtikettLogg BY SeqNr:
          IF Etikettlogg.Storl = "INFO" THEN DO:
/*               cBestTxt = ENTRY(1,Etikettlogg.Bongtekst,CHR(1)). */
              cButikTxt = IF NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)) > 2 THEN
                  ENTRY(3,Etikettlogg.Bongtekst,CHR(1)) ELSE "".
              cButikTxt = ChrConv(cButikTxt).
              cStartSlut = IF NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)) > 3 THEN
                  ENTRY(4,Etikettlogg.Bongtekst,CHR(1)) ELSE "".
              cStartSlut = REPLACE(cStartSlut,"SLUTT","SLUT").
              IF cStartSlut = "START" THEN
                  PUT UNFORMATTED
                      "^XA" SKIP
                      "^FO60,25" SKIP  /* ^FOx,y,z z= justification0=left 1=right */
                      "^A0N,40,40" SKIP /* zero scalable font ^A0N,x,x. height and width */
                      "^FD" cBestTxt SKIP
                      "^FO60,60" SKIP
                      "^A0N,40,40" SKIP
                      "^FD" cButikTxt "^FS" SKIP
                      "^MCY" SKIP
                      "^XZ" SKIP
                      "^XA" SKIP
                      "^FO80,60" SKIP  /* ^FOx,y,z z= justification0=left 1=right */
                      "^A0N,70,40" SKIP /* zero scalable font ^A0N,x,x. height and width */
                      "^FDSTART^FS" SKIP
                      "^MCY" SKIP
                      "^XZ" SKIP.
              ELSE IF cStartSlut = "SLUT" THEN
                  PUT UNFORMATTED
                      "^XA" SKIP
                      "^FO70,60" SKIP  /* ^FOx,y,z z= justification0=left 1=right */
                      "^A0N,80,40" SKIP /* zero scalable font ^A0N,x,x. height and width */
                      "^FDSLUT^FS" SKIP
                      "^MCY" SKIP
                      "^XZ" SKIP
                      "^XA" SKIP
                      "^FO1,25" SKIP  /* ^FOx,y,z z= justification0=left 1=right */
                      "^A0N,40,40" SKIP /* zero scalable font ^A0N,x,x. height and width */
                      "^FD" cBestTxt "^FS" SKIP
                      "^FO1,60" SKIP
                      "^A0N,40,40" SKIP
                      "^FD" cButikTxt "^FS" SKIP
                      "^MCY" SKIP
                      "^XZ" SKIP.
          END.               
          ELSE DO:
              FIND artbas WHERE artbas.vg = etikettlogg.vg AND
                                artbas.lopnr = etikettlogg.lopnr NO-LOCK NO-ERROR.
              IF AVAIL artbas THEN
                  cSasong = STRING(artbas.sasong,"999") + "   ".
              ELSE
                  cSasong = "".
              TStorlek = "".
              do ii = 1 to length(EtikettLogg.Storl):
                  if substring(EtikettLogg.Storl,ii,1) = " " then
                    TStorlek = TStorlek + "0". /* Space */
                  else if substring(EtikettLogg.Storl,ii,1) = "." then
                    NEXT. /* Punktum strippes bort. */
                  else if (substring(EtikettLogg.Storl,ii,1) < "0" or
                           substring(EtikettLogg.Storl,ii,1) > "9") then
                    TStorlek = TStorlek + "0". /* Alfanumeriske st|rrelser. */
                  else
                    TStorlek = TStorlek + substring(EtikettLogg.Storl,ii,1). /* Tar vare p} verdien */
              end.
              IF LENGTH(Tstorlek) = 4 THEN 
                  Tstorlek = SUBSTR(Tstorlek,2) + "0".
              ELSE
                  ASSIGN TStorlek = TRIM(TStorlek) /* Trimmer bort ledende space og gjør lengden = 4. */
                         TStorlek = (if LENGTH(TStorlek) = 2 then TStorlek + "0" else TStorlek)
                         TStorlek = "0" + TStorlek.
              ASSIGN cInterleave = STRING(EtikettLogg.vg,"999") + STRING(EtikettLogg.lopnr,"9999") + "0" + TStorlek
                     cStorl = LEFT-TRIM(Tstorlek,"0")
                     cStorl = SUBSTR(cStorl,1,LENGTH(cStorl) - 1) + "." + SUBSTR(cStorl,LENGTH(cStorl))
                     cTxt       = STRING(EtikettLogg.vg) + " - " + STRING(EtikettLogg.lopnr) + "   Stlk: " + 
              (IF NUM-ENTRIES(cStorl,".") = 2 AND ENTRY(2,cStorl,".") = "0" THEN ENTRY(1,cStorl,".") ELSE cStorl)
    /*               cPlu       = STRING(EtikettLogg.plunr) + CHR(13) */
                  cPlu       = "" + CHR(13)
                  cKr        = ENTRY(1,STRING(EtikettLogg.pris,"zzzz9.99"))
                  cOren      = ENTRY(2,STRING(EtikettLogg.pris,">>>9.99")).
/*                   cAntal     = '!P' + STRING(EtikettLogg.Ant) */
           DO iCount = 1 TO EtikettLogg.Ant:
               iTeller = iTeller + 1.
               IF iTeller > 9999 THEN
                   iTeller = 1.
               cTeller = STRING(iTeller,"9999").
               PUT UNFORMATTED                                          /*            PUT UNFORMATTED                              */
               "^XA" SKIP                                               /*            "^XA" SKIP                                   */
               "^FWR"           SKIP                                   /*            "^FO30,25^BY2" SKIP                          */

               "^FO330,25"       SKIP                                    /*            "^A0N,60,40" SKIP                            */
               "^A0N,60,40"     SKIP                                    /*            "^FD   Kr: " cKr "." cOren "^FS" SKIP        */
               "^FD   Kr: " cKr "." cOren "^FS" SKIP                    /*            "^PQ" STRING(EtikettLogg.Ant * 1) SKIP       */

               "^FO330,80^BY2" SKIP                                      /*            "^B2N,50,N,N,N" SKIP                         */
               "^B2N,50,N,N,N" SKIP                                     /*            "^FD" "    " cInterleave "^FS" SKIP          */
               "^FD" cInterleave "^FS" SKIP                             /*            "^FO1,70" SKIP                               */

               "^FO330,145" SKIP                                          /*            "^A0N,50,40" SKIP                            */
               "^A0N,60,25" SKIP                                        /*            "^FD" cTxt "^FS" SKIP                        */
               "^FD" cTxt "^FS" SKIP                                    /*            "^FO1,115" SKIP                              */
/*                "^FO435,15" SKIP                                         /* /*            "^PQ" STRING(EtikettLogg.Ant * 2) SKIP */ */ */
/*                "^GB30,155,30^FS"        SKIP                            /*            "^MCY" SKIP                                  */ */
               "^A0N,25,25" SKIP
               "^FO300,70^A0,20,30^FR^FD" cSasong cTeller "^FS" SKIP        /*            "^XZ" SKIP.                                  */
/*                /* LILLA */                                                                                                             */
               "^FO330,235"       SKIP                                    /*            "^A0N,60,40" SKIP                            */
               "^A0N,50,30"     SKIP                                    /*            "^FD   Kr: " cKr "." cOren "^FS" SKIP        */
               "^FD   Kr: " cKr "." cOren "^FS" SKIP                    /*            "^PQ" STRING(EtikettLogg.Ant * 1) SKIP       */
/*                                                                                                                                        */
               "^FO330,280^BY2" SKIP                                      /*            "^B2N,50,N,N,N" SKIP                         */
               "^B2N,40,N,N,N" SKIP                                     /*            "^FD" "    " cInterleave "^FS" SKIP          */
               "^FD" cInterleave "^FS" SKIP                             /*            "^FO1,70" SKIP                               */
/*                                                                                                                                        */
               "^FO330,325" SKIP                                          /*            "^A0N,50,40" SKIP                            */
               "^A0N,35,25" SKIP                                        /*            "^FD" cTxt "^FS" SKIP                        */
               "^FD" cTxt "^FS" SKIP                                    /*            "^FO1,115" SKIP                              */
/*                "^FO435,15" SKIP                                         /* /*            "^PQ" STRING(EtikettLogg.Ant * 2) SKIP */ */  */
/*                "^GB30,155,30^FS"        SKIP                            /*            "^MCY" SKIP                                  */  */
               "^A0N,25,25" SKIP
               "^FO300,295^A0,20,30^FR^FD" cTeller "^FS" SKIP        /*            "^XZ" SKIP.                                  */
                   

/*            "^FO138,15^A0,20,30^FR^FDSKOAUGUST^FS" SKIP */        /*            "^XZ" SKIP.                                  */
/*                "^PQ" STRING(EtikettLogg.Ant * 1) SKIP */
               "^MCY" SKIP
               "^XZ" SKIP.
           END.
          END.
    end.
    OUTPUT CLOSE.
    DO:
        OUTPUT to teller no-echo.
        IF iTeller > 9999 THEN
            iTeller = iTeller - 9999.
        EXPORT iTeller.
        OUTPUT close.
    END.
    EMPTY TEMP-TABLE etikettlogg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UtskriftDM3_LOrg) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtskriftDM3_LOrg Procedure 
PROCEDURE UtskriftDM3_LOrg :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cInterleave AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEan       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cTxt       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cPlu       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cKr        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cOren      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cAntal     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE ii         AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cStartSlut AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cBestTxt AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cButikTxt AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cStorl AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cGetPr AS CHARACTER   NO-UNDO.
/*     OUTPUT TO "CLIPBOARD".                       */
/*                                                  */
/*     FOR EACH etikettlogg:                        */
/*         DISP etikettlogg WITH 1 COL SIDE-LABELS. */
/*     END.                                         */
/*     OUTPUT CLOSE.                                */
/*     OUTPUT TO "CLIPBOARD".  */
/*     FOR EACH etikettlogg:   */
/*         EXPORT etikettlogg. */
/*     END.                    */
/*     OUTPUT CLOSE.           */
/*     RETURN.                 */
/*                             */
    cGetPr = SESSION:GET-PRINTERS().
    IF NOT CAN-DO(cGetPr,cPRINTER) THEN DO:
        cPRINTER = "*" + cPRINTER + "*".
        DO ii = 1 TO NUM-ENTRIES(cGetPr):
            IF ENTRY(ii,cGetPr) MATCHES cPRINTER THEN DO:
                cPRINTER = ENTRY(ii,cGetPr).
            END.
        END.
    END.
/* MESSAGE cPRINTER                       */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    IF NOT lTermklient THEN
        OUTPUT TO PRINTER VALUE (cPRINTER).
    ELSE
        OUTPUT TO VALUE (cPRINTER).
/*       PUT CONTROL wLayout. */
  /*   output close. */
    for each EtikettLogg BY SeqNr:
          IF Etikettlogg.Storl = "INFO" THEN DO:
/*               cBestTxt = ENTRY(1,Etikettlogg.Bongtekst,CHR(1)). */
              cButikTxt = IF NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)) > 2 THEN
                  ENTRY(3,Etikettlogg.Bongtekst,CHR(1)) ELSE "".
              cButikTxt = ChrConv(cButikTxt).
              cStartSlut = IF NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)) > 3 THEN
                  ENTRY(4,Etikettlogg.Bongtekst,CHR(1)) ELSE "".
              cStartSlut = REPLACE(cStartSlut,"SLUTT","SLUT").
              IF cStartSlut = "START" THEN
                  PUT UNFORMATTED
                      "^XA" SKIP
                      "^FO1,25" SKIP  /* ^FOx,y,z z= justification0=left 1=right */
                      "^A0N,40,40" SKIP /* zero scalable font ^A0N,x,x. height and width */
                      "^FD" cBestTxt SKIP
                      "^FO1,60" SKIP
                      "^A0N,40,40" SKIP
                      "^FD" cButikTxt "^FS" SKIP
                      "^MCY" SKIP
                      "^XZ" SKIP
                      "^XA" SKIP
                      "^FO80,60" SKIP  /* ^FOx,y,z z= justification0=left 1=right */
                      "^A0N,70,40" SKIP /* zero scalable font ^A0N,x,x. height and width */
                      "^FDSTART^FS" SKIP
                      "^MCY" SKIP
                      "^XZ" SKIP.
              ELSE IF cStartSlut = "SLUT" THEN
                  PUT UNFORMATTED
                      "^XA" SKIP
                      "^FO70,60" SKIP  /* ^FOx,y,z z= justification0=left 1=right */
                      "^A0N,80,40" SKIP /* zero scalable font ^A0N,x,x. height and width */
                      "^FDSLUT^FS" SKIP
                      "^MCY" SKIP
                      "^XZ" SKIP
                      "^XA" SKIP
                      "^FO1,25" SKIP  /* ^FOx,y,z z= justification0=left 1=right */
                      "^A0N,40,40" SKIP /* zero scalable font ^A0N,x,x. height and width */
                      "^FD" cBestTxt "^FS" SKIP
                      "^FO1,60" SKIP
                      "^A0N,40,40" SKIP
                      "^FD" cButikTxt "^FS" SKIP
                      "^MCY" SKIP
                      "^XZ" SKIP.
          END.               
          ELSE DO:
              TStorlek = "".
              do ii = 1 to length(EtikettLogg.Storl):
                  if substring(EtikettLogg.Storl,ii,1) = " " then
                    TStorlek = TStorlek + "0". /* Space */
                  else if substring(EtikettLogg.Storl,ii,1) = "." then
                    NEXT. /* Punktum strippes bort. */
                  else if (substring(EtikettLogg.Storl,ii,1) < "0" or
                           substring(EtikettLogg.Storl,ii,1) > "9") then
                    TStorlek = TStorlek + "0". /* Alfanumeriske st|rrelser. */
                  else
                    TStorlek = TStorlek + substring(EtikettLogg.Storl,ii,1). /* Tar vare p} verdien */
              end.
              ASSIGN TStorlek = TRIM(TStorlek) /* Trimmer bort ledende space og gjør lengden = 4. */
                     TStorlek = (if LENGTH(TStorlek) = 2 then TStorlek + "0" else TStorlek)
                     TStorlek = "0" + TStorlek.
           ASSIGN cInterleave = STRING(EtikettLogg.vg,"999") + STRING(EtikettLogg.lopnr,"9999") + "0" + TStorlek
                  cStorl = LEFT-TRIM(Tstorlek,"0")
                  cStorl = SUBSTR(cStorl,1,LENGTH(cStorl) - 1) + "." + SUBSTR(cStorl,LENGTH(cStorl))
                  cTxt       = STRING(EtikettLogg.vg) + "  " + STRING(EtikettLogg.lopnr) + "/" + 
               (IF NUM-ENTRIES(cStorl,".") = 2 AND ENTRY(2,cStorl,".") = "0" THEN ENTRY(1,cStorl,".") ELSE cStorl)
    /*               cPlu       = STRING(EtikettLogg.plunr) + CHR(13) */
                  cPlu       = "" + CHR(13)
                  cKr        = ENTRY(1,STRING(EtikettLogg.pris,"zzzz9.99"))
                  cOren      = ENTRY(2,STRING(EtikettLogg.pris,">>>9.99"))
                  cAntal     = '!P' + STRING(EtikettLogg.Ant)
           iTeller = iTeller + EtikettLogg.Ant.

           PUT UNFORMATTED
           "^XA" SKIP
           "^FO30,25" SKIP     /* position */
           "^A0N,60,40" SKIP
           "^FDKr:    " cKr "." cOren "^FS" SKIP
           "^FO30,115^BY2" SKIP
           "^B2N,50,N,N,N" SKIP
           "^FD" "    " cInterleave "^FS" SKIP
           "^FO1,70" SKIP
           "^A0N,50,40" SKIP
           "^FD" cTxt "^FS" SKIP
           "^PQ" STRING(EtikettLogg.Ant * 1) SKIP
/*            "^PQ" STRING(EtikettLogg.Ant * 2) SKIP */
           "^MCY" SKIP
           "^XZ" SKIP.
/*            PUT UNFORMATTED                              */
/*            "^XA" SKIP                                   */
/*            "^FO30,25^BY2" SKIP                          */
/*            "^B2N,50,N,N,N" SKIP                         */
/*            "^FD" "    " cInterleave "^FS" SKIP          */
/*            "^FO1,70" SKIP                               */
/*            "^A0N,50,40" SKIP                            */
/*            "^FD" cTxt "^FS" SKIP                        */
/*            "^FO1,115" SKIP                              */
/*            "^A0N,60,40" SKIP                            */
/*            "^FD   Kr: " cKr "." cOren "^FS" SKIP        */
/*            "^PQ" STRING(EtikettLogg.Ant * 1) SKIP       */
/* /*            "^PQ" STRING(EtikettLogg.Ant * 2) SKIP */ */
/*            "^MCY" SKIP                                  */
/*            "^XZ" SKIP.                                  */
          END.
    end.
    OUTPUT CLOSE.
    EMPTY TEMP-TABLE etikettlogg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UtskriftMARKPOINT_0) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtskriftMARKPOINT_0 Procedure 
PROCEDURE UtskriftMARKPOINT_0 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTeller AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIterleave AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEan       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cTxt       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cPlu       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cKr        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cOren      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cAntal     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE ii         AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cTellerCmd AS CHARACTER   NO-UNDO.
    IF SEARCH("teller") <> ? THEN
    DO:
        INPUT from teller no-echo.
        IMPORT UNFORMATTED cTeller.
        ASSIGN iTeller = INT(cTeller).
/*             set iTeller. */
        INPUT close.
        IF (iTeller = 0 OR iTeller > 9999) THEN iTeller = 1.
    END.
    ELSE iTeller = 1.
/*     OUTPUT TO "CLIPBOARD".                       */
/*                                                  */
/*     FOR EACH etikettlogg:                        */
/*         DISP etikettlogg WITH 1 COL SIDE-LABELS. */
/*     END.                                         */
/*     OUTPUT CLOSE.                                */
    cTellerCmd = "!C" + CHR(13) + "!N1 " + STRING(iTeller) + " 1 4 1" + CHR(13).
    IF NOT lTermklient THEN
        OUTPUT TO PRINTER VALUE (cPRINTER).
    ELSE
        OUTPUT TO VALUE (cPRINTER).
    PUT CONTROL cTellerCmd.
      PUT CONTROL wLayout.

  /*   output close. */
    for each EtikettLogg
       break by EtikettLogg.butik
             BY EtikettLogg.vg
             by EtikettLogg.lopnr
             by EtikettLogg.Storl:
          TStorlek = "".
          do ii = 1 to length(EtikettLogg.Storl):
              if substring(EtikettLogg.Storl,ii,1) = " " then
                TStorlek = TStorlek + "0". /* Space */
              else if substring(EtikettLogg.Storl,ii,1) = "." then
                NEXT. /* Punktum strippes bort. */
              else if (substring(EtikettLogg.Storl,ii,1) < "0" or
                       substring(EtikettLogg.Storl,ii,1) > "9") then
                TStorlek = TStorlek + "0". /* Alfanumeriske st|rrelser. */
              else
                TStorlek = TStorlek + substring(EtikettLogg.Storl,ii,1). /* Tar vare p} verdien */
          end.
          ASSIGN TStorlek = TRIM(TStorlek) /* Trimmer bort ledende space og gjør lengden = 4. */
                 TStorlek = (if LENGTH(TStorlek) = 2 then TStorlek + "0" else TStorlek)
                 TStorlek = "0" + TStorlek.
       ASSIGN cIterleave = STRING(EtikettLogg.vg,"999") + STRING(EtikettLogg.lopnr,"9999") + "0" + TStorlek + CHR(13)
/*               cEan       = (IF EtikettLogg.Ean = "0" THEN "" ELSE EtikettLogg.Ean) + CHR(13) */
              cEan       = "" + CHR(13)
              cTxt       = STRING(EtikettLogg.vg) + " - " + STRING(EtikettLogg.lopnr) + "   stl: " + EtikettLogg.Storl + CHR(13)
/*               cPlu       = STRING(EtikettLogg.plunr) + CHR(13) */
              cPlu       = "" + CHR(13)
              cKr        = ENTRY(1,STRING(EtikettLogg.pris,"zzzz9.99")) + CHR(13)
              cOren      = ENTRY(2,STRING(EtikettLogg.pris,">>>9.99")) + CHR(13)
              cAntal     = '!P' + STRING(EtikettLogg.Ant) + CHR(13).
       iTeller = iTeller + EtikettLogg.Ant.
       put control cIterleave.
       put control cTxt.
       put control cKr.
       put control cOren.
/*        PUT CONTROL "1". */
/*        put control cIterleave. */
/*        put control cTxt.       */
/*        PUT CONTROL "1".        */
/*        put control cKr.        */
/*        put control cOren.      */
       PUT CONTROL cAntal.
    end.
    OUTPUT CLOSE.
    DO:
        OUTPUT to teller no-echo.
        IF iTeller > 9999 THEN
            iTeller = iTeller - 9999.
        EXPORT iTeller.
        OUTPUT close.
    END.
    EMPTY TEMP-TABLE etikettlogg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UtskriftMARKPOINT_1) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtskriftMARKPOINT_1 Procedure 
PROCEDURE UtskriftMARKPOINT_1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTeller AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIterleave AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEan       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cTxt       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cPlu       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cKr        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cOren      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cAntal     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE ii         AS INTEGER     NO-UNDO.

    OUTPUT TO "CLIPBOARD".
    
    FOR EACH etikettlogg:
        DISP etikettlogg WITH 1 COL SIDE-LABELS.
    END.
    OUTPUT CLOSE.
    
    IF NOT lTermklient THEN
        OUTPUT TO PRINTER VALUE (cPRINTER).
    ELSE
        OUTPUT TO VALUE (cPRINTER).
      put control wLayout.
  /*   output close. */
    for each EtikettLogg
       break by EtikettLogg.vg
             by EtikettLogg.lopnr
             by EtikettLogg.Storl:
          TStorlek = "".
          do ii = 1 to length(EtikettLogg.Storl):
              if substring(EtikettLogg.Storl,ii,1) = " " then
                TStorlek = TStorlek + "0". /* Space */
              else if substring(EtikettLogg.Storl,ii,1) = "." then
                NEXT. /* Punktum strippes bort. */
              else if (substring(EtikettLogg.Storl,ii,1) < "0" or
                       substring(EtikettLogg.Storl,ii,1) > "9") then
                TStorlek = TStorlek + "0". /* Alfanumeriske st|rrelser. */
              else
                TStorlek = TStorlek + substring(EtikettLogg.Storl,ii,1). /* Tar vare p} verdien */
          end.
          ASSIGN TStorlek = TRIM(TStorlek) /* Trimmer bort ledende space og gjør lengden = 4. */
                 TStorlek = (if LENGTH(TStorlek) = 2 then TStorlek + "0" else TStorlek)
                 TStorlek = "0" + TStorlek.
       ASSIGN cIterleave = STRING(EtikettLogg.vg,"999") + STRING(EtikettLogg.lopnr,"9999") + TStorlek + CHR(13)
/*               cEan       = (IF EtikettLogg.Ean = "0" THEN "" ELSE EtikettLogg.Ean) + CHR(13) */
              cEan       = "" + CHR(13)
              cTxt       = STRING(EtikettLogg.vg) + " / " + STRING(EtikettLogg.lopnr) + " stlk " + EtikettLogg.Storl + CHR(13)
/*               cPlu       = STRING(EtikettLogg.plunr) + CHR(13) */
              cPlu       = "" + CHR(13)
              cKr        = ENTRY(1,STRING(EtikettLogg.pris,"zzz9.99")) + CHR(13)
              cOren      = ENTRY(2,STRING(EtikettLogg.pris,">>>9.99")) + CHR(13)
              cAntal     = '!P' + STRING(EtikettLogg.Ant) + CHR(13).
       put control cIterleave.
       put control cEan.
       put control cTxt.
       put control cPlu.
       put control cKr.
       put control cOren.
       PUT CONTROL cAntal.
    end.
    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UtskriftMARKPOINT_2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtskriftMARKPOINT_2 Procedure 
PROCEDURE UtskriftMARKPOINT_2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTeller AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIterleave AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEan       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cTxt       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cPlu       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cKr        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cOren      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cAntal     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE ii         AS INTEGER     NO-UNDO.
    IF NOT lTermklient THEN
        OUTPUT TO PRINTER VALUE (cPRINTER).
    ELSE
        OUTPUT TO VALUE (cPRINTER).
      put control wLayout.
  /*   output close. */
    for each EtikettLogg
       break by EtikettLogg.vg
             by EtikettLogg.lopnr
             by EtikettLogg.Storl:

          TStorlek = "".
          do i = 1 to length(EtikettLogg.Storl):
              if substring(EtikettLogg.Storl,ii,1) = " " then
                TStorlek = TStorlek + "0". /* Space */
              else if substring(EtikettLogg.Storl,ii,1) = "." then
                NEXT. /* Punktum strippes bort. */
              else if (substring(EtikettLogg.Storl,ii,1) < "0" or
                       substring(EtikettLogg.Storl,ii,1) > "9") then
                TStorlek = TStorlek + "0". /* Alfanumeriske st|rrelser. */
              else
                TStorlek = TStorlek + substring(EtikettLogg.Storl,ii,1). /* Tar vare p} verdien */
          end.
          ASSIGN TStorlek = TRIM(TStorlek) /* Trimmer bort ledende space og gjør lengden = 4. */
                 TStorlek = (if LENGTH(TStorlek) = 2 then TStorlek + "0" else TStorlek)
                 TStorlek = "0" + TStorlek.
       ASSIGN cIterleave = STRING(EtikettLogg.vg,"999") + STRING(EtikettLogg.lopnr,"9999") + "0" + TStorlek + CHR(13)
              cEan       = "" + CHR(13)
/*               cEan       = (IF EtikettLogg.Ean = "0" THEN "" ELSE EtikettLogg.Ean) + CHR(13) */
              cTxt       = STRING(EtikettLogg.vg) + " / " + STRING(EtikettLogg.lopnr) + " stlk " + EtikettLogg.Storl + CHR(13)
/*               cPlu       = STRING(EtikettLogg.plunr) + CHR(13) */
              cPlu       = "" + CHR(13)
              cKr        = ENTRY(1,STRING(EtikettLogg.pris,"zzz9.99")) + CHR(13)
              cOren      = ENTRY(2,STRING(EtikettLogg.pris,">>>9.99")) + CHR(13)
              cAntal     = '!P' + STRING(EtikettLogg.Ant) + CHR(13).
       put control cIterleave.
       put control cEan.
       put control cTxt.
       put control cPlu.
       put control cKr.
       put control cOren.
       PUT CONTROL cAntal.
    end.
    OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UtskriftMARKPOINT_L) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtskriftMARKPOINT_L Procedure 
PROCEDURE UtskriftMARKPOINT_L :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cTeller AS CHARACTER  NO-UNDO.
    DEFINE VARIABLE cIterleave AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEan       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cTxt       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cPlu       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cKr        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cOren      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cAntal     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE ii         AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cTellerCmd AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cGetPr AS CHARACTER   NO-UNDO.
    cGetPr = SESSION:GET-PRINTERS().
    IF NOT CAN-DO(cGetPr,cPRINTER) THEN DO:
        cPRINTER = "*" + cPRINTER + "*".
        DO ii = 1 TO NUM-ENTRIES(cGetPr):
            IF ENTRY(ii,cGetPr) MATCHES cPRINTER THEN DO:
                cPRINTER = ENTRY(ii,cGetPr).
            END.
        END.
    END.

    IF SEARCH("teller") <> ? THEN
    DO:
        INPUT from teller no-echo.
        IMPORT UNFORMATTED cTeller.
        ASSIGN iTeller = INT(cTeller).
/*             set iTeller. */
        INPUT close.
        IF (iTeller = 0 OR iTeller > 9999) THEN iTeller = 1.
    END.
    ELSE iTeller = 1.
/*     OUTPUT TO "CLIPBOARD".                       */
/*                                                  */
/*     FOR EACH etikettlogg:                        */
/*         DISP etikettlogg WITH 1 COL SIDE-LABELS. */
/*     END.                                         */
/*     OUTPUT CLOSE.                                */
    cTellerCmd = "!C" + CHR(13) + "!N1 " + STRING(iTeller) + " 1 4 1" + CHR(13).
    IF NOT lTermklient THEN
        OUTPUT TO PRINTER VALUE (cPRINTER).
    ELSE
        OUTPUT TO VALUE (cPRINTER).
    PUT CONTROL cTellerCmd.
      PUT CONTROL wLayout.

  /*   output close. */
      for each EtikettLogg
         break by EtikettLogg.butik
               BY EtikettLogg.vg
               by EtikettLogg.lopnr
               by EtikettLogg.Storl:
          TStorlek = "".
          do ii = 1 to length(EtikettLogg.Storl):
              if substring(EtikettLogg.Storl,ii,1) = " " then
                TStorlek = TStorlek + "0". /* Space */
              else if substring(EtikettLogg.Storl,ii,1) = "." then
                NEXT. /* Punktum strippes bort. */
              else if (substring(EtikettLogg.Storl,ii,1) < "0" or
                       substring(EtikettLogg.Storl,ii,1) > "9") then
                TStorlek = TStorlek + "0". /* Alfanumeriske st|rrelser. */
              else
                TStorlek = TStorlek + substring(EtikettLogg.Storl,ii,1). /* Tar vare p} verdien */
          end.
          ASSIGN TStorlek = TRIM(TStorlek) /* Trimmer bort ledende space og gjør lengden = 4. */
                 TStorlek = (if LENGTH(TStorlek) = 2 then TStorlek + "0" else TStorlek)
                 TStorlek = "0" + TStorlek.
       ASSIGN cIterleave = STRING(EtikettLogg.vg,"999") + STRING(EtikettLogg.lopnr,"9999") + "0" + TStorlek + CHR(13)
/*               cEan       = (IF EtikettLogg.Ean = "0" THEN "" ELSE EtikettLogg.Ean) + CHR(13) */
              cEan       = "" + CHR(13)
              cTxt       = STRING(EtikettLogg.vg) + " - " + STRING(EtikettLogg.lopnr) + "   stl: " + EtikettLogg.Storl + CHR(13)
/*               cPlu       = STRING(EtikettLogg.plunr) + CHR(13) */
              cPlu       = "" + CHR(13)
              cKr        = ENTRY(1,STRING(EtikettLogg.pris,"zzzz9.99")) + CHR(13)
              cOren      = ENTRY(2,STRING(EtikettLogg.pris,">>>9.99")) + CHR(13)
              cAntal     = '!P' + STRING(EtikettLogg.Ant) + CHR(13).
       iTeller = iTeller + EtikettLogg.Ant.
       put control cIterleave.
       put control cTxt.
       put control cKr.
       put control cOren.
/*        PUT CONTROL "1". */
/*        put control cIterleave. */
/*        put control cTxt.       */
/*        PUT CONTROL "1".        */
/*        put control cKr.        */
/*        put control cOren.      */
       PUT CONTROL cAntal.
    end.
    OUTPUT CLOSE.
    DO:
        OUTPUT to teller no-echo.
        IF iTeller > 9999 THEN
            iTeller = iTeller - 9999.
        EXPORT iTeller.
        OUTPUT close.
    END.
    EMPTY TEMP-TABLE etikettlogg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UtskriftS4M) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtskriftS4M Procedure 
PROCEDURE UtskriftS4M :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cInterleave AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEan       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cTxt       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cPlu       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cKr        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cOren      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cAntal     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE ii         AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cStartSlut AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cBestTxt AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cButikTxt AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cStorl AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cGetPr AS CHARACTER   NO-UNDO.
/*     OUTPUT TO "CLIPBOARD".                       */
/*                                                  */
/*     FOR EACH etikettlogg:                        */
/*         DISP etikettlogg WITH 1 COL SIDE-LABELS. */
/*     END.                                         */
/*     OUTPUT CLOSE.                                */
/*     OUTPUT TO "CLIPBOARD".  */
/*     FOR EACH etikettlogg:   */
/*         EXPORT etikettlogg. */
/*     END.                    */
/*     OUTPUT CLOSE.           */
/*     RETURN.                 */
/*                             */
    cGetPr = SESSION:GET-PRINTERS().
    IF NOT CAN-DO(cGetPr,cPRINTER) THEN DO:
        cPRINTER = "*" + cPRINTER + "*".
        DO ii = 1 TO NUM-ENTRIES(cGetPr):
            IF ENTRY(ii,cGetPr) MATCHES cPRINTER THEN DO:
                cPRINTER = ENTRY(ii,cGetPr).
            END.
        END.
    END.
/* MESSAGE cPRINTER                       */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
    IF NOT lTermklient THEN
        OUTPUT TO PRINTER VALUE (cPRINTER).
    ELSE
        OUTPUT TO VALUE (cPRINTER).
/*       PUT CONTROL wLayout. */
  /*   output close. */
    for each EtikettLogg BY SeqNr:
          IF Etikettlogg.Storl = "INFO" THEN DO:
/*               cBestTxt = ENTRY(1,Etikettlogg.Bongtekst,CHR(1)). */
              cButikTxt = IF NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)) > 2 THEN
                  ENTRY(3,Etikettlogg.Bongtekst,CHR(1)) ELSE "".
              cButikTxt = ChrConv(cButikTxt).
              cStartSlut = IF NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)) > 3 THEN
                  ENTRY(4,Etikettlogg.Bongtekst,CHR(1)) ELSE "".
              cStartSlut = REPLACE(cStartSlut,"SLUTT","SLUT").
              IF cStartSlut = "START" THEN
                  PUT UNFORMATTED
                      "^XA" SKIP
                      "^FO1,25" SKIP  /* ^FOx,y,z z= justification0=left 1=right */
                      "^A0N,40,40" SKIP /* zero scalable font ^A0N,x,x. height and width */
                      "^FD" cBestTxt SKIP
                      "^FO1,60" SKIP
                      "^A0N,40,40" SKIP
                      "^FD" cButikTxt "^FS" SKIP
                      "^MCY" SKIP
                      "^XZ" SKIP
                      "^XA" SKIP
                      "^FO80,60" SKIP  /* ^FOx,y,z z= justification0=left 1=right */
                      "^A0N,70,40" SKIP /* zero scalable font ^A0N,x,x. height and width */
                      "^FDSTART^FS" SKIP
                      "^MCY" SKIP
                      "^XZ" SKIP.
              ELSE IF cStartSlut = "SLUT" THEN
                  PUT UNFORMATTED
                      "^XA" SKIP
                      "^FO70,60" SKIP  /* ^FOx,y,z z= justification0=left 1=right */
                      "^A0N,80,40" SKIP /* zero scalable font ^A0N,x,x. height and width */
                      "^FDSLUT^FS" SKIP
                      "^MCY" SKIP
                      "^XZ" SKIP
                      "^XA" SKIP
                      "^FO1,25" SKIP  /* ^FOx,y,z z= justification0=left 1=right */
                      "^A0N,40,40" SKIP /* zero scalable font ^A0N,x,x. height and width */
                      "^FD" cBestTxt "^FS" SKIP
                      "^FO1,60" SKIP
                      "^A0N,40,40" SKIP
                      "^FD" cButikTxt "^FS" SKIP
                      "^MCY" SKIP
                      "^XZ" SKIP.
          END.               
          ELSE DO:
              TStorlek = "".
              do ii = 1 to length(EtikettLogg.Storl):
                  if substring(EtikettLogg.Storl,ii,1) = " " then
                    TStorlek = TStorlek + "0". /* Space */
                  else if substring(EtikettLogg.Storl,ii,1) = "." then
                    NEXT. /* Punktum strippes bort. */
                  else if (substring(EtikettLogg.Storl,ii,1) < "0" or
                           substring(EtikettLogg.Storl,ii,1) > "9") then
                    TStorlek = TStorlek + "0". /* Alfanumeriske st|rrelser. */
                  else
                    TStorlek = TStorlek + substring(EtikettLogg.Storl,ii,1). /* Tar vare p} verdien */
              end.
              ASSIGN TStorlek = TRIM(TStorlek) /* Trimmer bort ledende space og gjør lengden = 4. */
                     TStorlek = (if LENGTH(TStorlek) = 2 then TStorlek + "0" else TStorlek)
                     TStorlek = "0" + TStorlek.
           ASSIGN cInterleave = STRING(EtikettLogg.vg,"999") + STRING(EtikettLogg.lopnr,"9999") + "0" + TStorlek
                  cStorl = LEFT-TRIM(Tstorlek,"0")
                  cStorl = SUBSTR(cStorl,1,LENGTH(cStorl) - 1) + "." + SUBSTR(cStorl,LENGTH(cStorl))
                  cTxt       = STRING(EtikettLogg.vg) + "  " + STRING(EtikettLogg.lopnr) + "/" + 
               (IF NUM-ENTRIES(cStorl,".") = 2 AND ENTRY(2,cStorl,".") = "0" THEN ENTRY(1,cStorl,".") ELSE cStorl)
    /*               cPlu       = STRING(EtikettLogg.plunr) + CHR(13) */
                  cPlu       = "" + CHR(13)
                  cKr        = ENTRY(1,STRING(EtikettLogg.pris,"zzzz9.99"))
                  cOren      = ENTRY(2,STRING(EtikettLogg.pris,">>>9.99"))
                  cAntal     = '!P' + STRING(EtikettLogg.Ant)
           iTeller = iTeller + EtikettLogg.Ant.

           PUT UNFORMATTED
           "^XA" SKIP
           "^FO1,5^BY2" SKIP
           "^B2N,60,N,N,N" SKIP
           "^FD" cInterleave "^FS" SKIP
           "^FO1,70" SKIP
           "^A0N,50,40" SKIP
           "^FD" cTxt "^FS" SKIP
           "^FO1,115" SKIP
           "^A0N,60,40" SKIP
           "^FD   Kr: " cKr "." cOren "^FS" SKIP
           "^PQ" STRING(EtikettLogg.Ant * 2) SKIP
           "^MCY" SKIP
           "^XZ" SKIP.
          END.
    end.
    OUTPUT CLOSE.
    EMPTY TEMP-TABLE etikettlogg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UtskriftS4MA) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtskriftS4MA Procedure 
PROCEDURE UtskriftS4MA :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cInterleave AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEan       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cTxt       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cPlu       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cKr        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cOren      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cAntal     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE ii         AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cStartSlut AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cBestTxt AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cButikTxt AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cStorl AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cGetPr AS CHARACTER   NO-UNDO.
    cPRINTER = "*" + cPRINTER + "*".
    cGetPr = SESSION:GET-PRINTERS().
    DO ii = 1 TO NUM-ENTRIES(cGetPr):
        IF ENTRY(ii,cGetPr) MATCHES cPRINTER THEN DO:
            cPRINTER = ENTRY(ii,cGetPr).
        END.
    END.
    /*     OUTPUT TO "CLIPBOARD".                       */
/*                                                  */
/*     FOR EACH etikettlogg:                        */
/*         DISP etikettlogg WITH 1 COL SIDE-LABELS. */
/*     END.                                         */
/*     OUTPUT CLOSE.                                */
/*     OUTPUT TO "C:\tmp\etikett_sort.txt" APPEND. */
/*     FOR EACH etikettlogg BY seqnr:              */
/*         DISP etikettlogg.vg                     */
/*             etikettlogg.Lopnr                   */
/*             etikettlogg.Ant                     */
/*             etikettlogg.Storl  FORMAT "x(5)"    */
/*             LENGTH(etikettlogg.Storl)           */
/*             etikettlogg.seqnr FORMAT "9999".    */
/*     END.                                        */
/*     OUTPUT CLOSE.                               */
/*     RETURN.                 */
/*                             */
    IF NOT lTermklient THEN
        OUTPUT TO PRINTER VALUE (cPRINTER).
    ELSE
        OUTPUT TO VALUE (cPRINTER).
/*       PUT CONTROL wLayout. */
  /*   output close. */
/*         for each EtikettLogg BY SeqNr: */
    for each EtikettLogg BY Etikettlogg.Seqnr:
          IF Etikettlogg.Storl = "INFO" THEN DO:
/*               cBestTxt = ENTRY(1,Etikettlogg.Bongtekst,CHR(1)). */
              cButikTxt = IF NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)) > 2 THEN
                  ENTRY(3,Etikettlogg.Bongtekst,CHR(1)) ELSE "".
              cButikTxt = ChrConv(cButikTxt).
              cStartSlut = IF NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)) > 3 THEN
                  ENTRY(4,Etikettlogg.Bongtekst,CHR(1)) ELSE "".
              cStartSlut = REPLACE(cStartSlut,"SLUTT","SLUT").
              IF cStartSlut = "START" THEN
                  PUT UNFORMATTED
                      "^XA" SKIP
                      "^FO1,25" SKIP  /* ^FOx,y,z z= justification0=left 1=right */
                      "^A0N,40,40" SKIP /* zero scalable font ^A0N,x,x. height and width */
                      "^FD" cBestTxt SKIP
                      "^FO1,60" SKIP
                      "^A0N,40,40" SKIP
                      "^FD" cButikTxt "^FS" SKIP
                      "^MCY" SKIP
                      "^XZ" SKIP
                      "^XA" SKIP
                      "^FO80,60" SKIP  /* ^FOx,y,z z= justification0=left 1=right */
                      "^A0N,70,40" SKIP /* zero scalable font ^A0N,x,x. height and width */
                      "^FDSTART^FS" SKIP
                      "^MCY" SKIP
                      "^XZ" SKIP.
              ELSE IF cStartSlut = "SLUT" THEN
                  PUT UNFORMATTED
                      "^XA" SKIP
                      "^FO70,60" SKIP  /* ^FOx,y,z z= justification0=left 1=right */
                      "^A0N,80,40" SKIP /* zero scalable font ^A0N,x,x. height and width */
                      "^FDSLUT^FS" SKIP
                      "^MCY" SKIP
                      "^XZ" SKIP
                      "^XA" SKIP
                      "^FO1,25" SKIP  /* ^FOx,y,z z= justification0=left 1=right */
                      "^A0N,40,40" SKIP /* zero scalable font ^A0N,x,x. height and width */
                      "^FD" cBestTxt "^FS" SKIP
                      "^FO1,60" SKIP
                      "^A0N,40,40" SKIP
                      "^FD" cButikTxt "^FS" SKIP
                      "^MCY" SKIP
                      "^XZ" SKIP.
          END.               
          ELSE DO:
              TStorlek = "".
              do ii = 1 to length(EtikettLogg.Storl):
                  if substring(EtikettLogg.Storl,ii,1) = " " then
                    TStorlek = TStorlek + "0". /* Space */
                  else if substring(EtikettLogg.Storl,ii,1) = "." then
                    NEXT. /* Punktum strippes bort. */
                  else if (substring(EtikettLogg.Storl,ii,1) < "0" or
                           substring(EtikettLogg.Storl,ii,1) > "9") then
                    TStorlek = TStorlek + "0". /* Alfanumeriske st|rrelser. */
                  else
                    TStorlek = TStorlek + substring(EtikettLogg.Storl,ii,1). /* Tar vare p} verdien */
              end.
              IF LENGTH(Tstorlek) = 4 THEN 
                  Tstorlek = SUBSTR(Tstorlek,2) + "0".
              ELSE
                  ASSIGN TStorlek = TRIM(TStorlek) /* Trimmer bort ledende space og gjør lengden = 4. */
                         TStorlek = (if LENGTH(TStorlek) = 2 then TStorlek + "0" else TStorlek)
                         TStorlek = "0" + TStorlek.
              ASSIGN cInterleave = STRING(EtikettLogg.vg,"999") + STRING(EtikettLogg.lopnr,"9999") + "0" + TStorlek
                     cStorl = LEFT-TRIM(Tstorlek,"0")
                     cStorl = SUBSTR(cStorl,1,LENGTH(cStorl) - 1) + "." + SUBSTR(cStorl,LENGTH(cStorl))
                     cTxt       = STRING(EtikettLogg.vg) + " " + STRING(EtikettLogg.lopnr) + "   Stlk:" + 
              (IF NUM-ENTRIES(cStorl,".") = 2 AND ENTRY(2,cStorl,".") = "0" THEN ENTRY(1,cStorl,".") ELSE cStorl)
    /*               cPlu       = STRING(EtikettLogg.plunr) + CHR(13) */
                  cPlu       = "" + CHR(13)
                  cKr        = ENTRY(1,STRING(EtikettLogg.pris,"zzzz9.99"))
                  cOren      = ENTRY(2,STRING(EtikettLogg.pris,">>>9.99"))
                  cAntal     = '!P' + STRING(EtikettLogg.Ant)
           iTeller = iTeller + EtikettLogg.Ant.
           PUT UNFORMATTED
           "^XA" SKIP
           "^FWR"                    SKIP
           "^FO160,20^BY2" SKIP
           "^B2N,45,N,N,N" SKIP
           "^FD" cInterleave "^FS" SKIP
           "^FO160,75" SKIP
           "^A0N,45,30" SKIP
           "^FD" cTxt "^FS" SKIP
           "^FO160,120" SKIP
           "^A0N,60,40" SKIP
           "^FD   Kr: " cKr "." cOren "^FS" SKIP
           "^FO420,20" SKIP
           "^GB30,155,30^FS"        SKIP
           "^FO420,20^A0,20,30^FR^FDSKOAUGUST^FS" SKIP
           "^PQ" STRING(EtikettLogg.Ant * 2) SKIP
           "^MCY" SKIP
           "^XZ" SKIP.
           /*
           PUT UNFORMATTED
           "^XA" SKIP
           "^FWR"                    SKIP
           "^FO281,20^BY2" SKIP
           "^B2N,45,N,N,N" SKIP
           "^FD" cInterleave "^FS" SKIP
           "^FO281,75" SKIP
           "^A0N,45,30" SKIP
           "^FD" cTxt "^FS" SKIP
           "^FO281,120" SKIP
           "^A0N,60,40" SKIP
           "^FD   Kr: " cKr "." cOren "^FS" SKIP
           "^FO535,15" SKIP
           "^GB30,155,30^FS"        SKIP
           "^FO538,15^A0,20,30^FR^FDSKOAUGUST^FS" SKIP
           "^PQ" STRING(EtikettLogg.Ant * 2) SKIP
           "^MCY" SKIP
           "^XZ" SKIP.
           */
          END.
    end.
    OUTPUT CLOSE.
    EMPTY TEMP-TABLE etikettlogg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UtskriftS4MOrg2) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UtskriftS4MOrg2 Procedure 
PROCEDURE UtskriftS4MOrg2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cInterleave AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cEan       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cTxt       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cPlu       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cKr        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cOren      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cAntal     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE ii         AS INTEGER     NO-UNDO.
    DEFINE VARIABLE cStartSlut AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cBestTxt AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cButikTxt AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cStorl AS CHARACTER   NO-UNDO.
/*     OUTPUT TO "CLIPBOARD".                       */
/*                                                  */
/*     FOR EACH etikettlogg:                        */
/*         DISP etikettlogg WITH 1 COL SIDE-LABELS. */
/*     END.                                         */
/*     OUTPUT CLOSE.                                */
/*     OUTPUT TO "CLIPBOARD".  */
/*     FOR EACH etikettlogg:   */
/*         EXPORT etikettlogg. */
/*     END.                    */
/*     OUTPUT CLOSE.           */
/*     RETURN.                 */
/*                             */
    IF NOT lTermklient THEN
        OUTPUT TO PRINTER VALUE (cPRINTER).
    ELSE
        OUTPUT TO VALUE (cPRINTER).
/*       PUT CONTROL wLayout. */
  /*   output close. */
    for each EtikettLogg BY SeqNr:
          IF Etikettlogg.Storl = "INFO" THEN DO:
/*               cBestTxt = ENTRY(1,Etikettlogg.Bongtekst,CHR(1)). */
              cButikTxt = IF NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)) > 2 THEN
                  ENTRY(3,Etikettlogg.Bongtekst,CHR(1)) ELSE "".
              cButikTxt = ChrConv(cButikTxt).
              cStartSlut = IF NUM-ENTRIES(Etikettlogg.Bongtekst,CHR(1)) > 3 THEN
                  ENTRY(4,Etikettlogg.Bongtekst,CHR(1)) ELSE "".
              cStartSlut = REPLACE(cStartSlut,"SLUTT","SLUT").
              IF cStartSlut = "START" THEN
                  PUT UNFORMATTED
                      "^XA" SKIP
                      "^FO1,25" SKIP  /* ^FOx,y,z z= justification0=left 1=right */
                      "^A0N,40,40" SKIP /* zero scalable font ^A0N,x,x. height and width */
                      "^FD" cBestTxt SKIP
                      "^FO1,60" SKIP
                      "^A0N,40,40" SKIP
                      "^FD" cButikTxt "^FS" SKIP
                      "^MCY" SKIP
                      "^XZ" SKIP
                      "^XA" SKIP
                      "^FO80,60" SKIP  /* ^FOx,y,z z= justification0=left 1=right */
                      "^A0N,70,40" SKIP /* zero scalable font ^A0N,x,x. height and width */
                      "^FDSTART^FS" SKIP
                      "^MCY" SKIP
                      "^XZ" SKIP.
              ELSE IF cStartSlut = "SLUT" THEN
                  PUT UNFORMATTED
                      "^XA" SKIP
                      "^FO70,60" SKIP  /* ^FOx,y,z z= justification0=left 1=right */
                      "^A0N,80,40" SKIP /* zero scalable font ^A0N,x,x. height and width */
                      "^FDSLUT^FS" SKIP
                      "^MCY" SKIP
                      "^XZ" SKIP
                      "^XA" SKIP
                      "^FO1,25" SKIP  /* ^FOx,y,z z= justification0=left 1=right */
                      "^A0N,40,40" SKIP /* zero scalable font ^A0N,x,x. height and width */
                      "^FD" cBestTxt "^FS" SKIP
                      "^FO1,60" SKIP
                      "^A0N,40,40" SKIP
                      "^FD" cButikTxt "^FS" SKIP
                      "^MCY" SKIP
                      "^XZ" SKIP.
          END.               
          ELSE DO:
              TStorlek = "".
              do ii = 1 to length(EtikettLogg.Storl):
                  if substring(EtikettLogg.Storl,ii,1) = " " then
                    TStorlek = TStorlek + "0". /* Space */
                  else if substring(EtikettLogg.Storl,ii,1) = "." then
                    NEXT. /* Punktum strippes bort. */
                  else if (substring(EtikettLogg.Storl,ii,1) < "0" or
                           substring(EtikettLogg.Storl,ii,1) > "9") then
                    TStorlek = TStorlek + "0". /* Alfanumeriske st|rrelser. */
                  else
                    TStorlek = TStorlek + substring(EtikettLogg.Storl,ii,1). /* Tar vare p} verdien */
              end.
              ASSIGN TStorlek = TRIM(TStorlek) /* Trimmer bort ledende space og gjør lengden = 4. */
                     TStorlek = (if LENGTH(TStorlek) = 2 then TStorlek + "0" else TStorlek)
                     TStorlek = "0" + TStorlek.
           ASSIGN cInterleave = STRING(EtikettLogg.vg,"999") + STRING(EtikettLogg.lopnr,"9999") + "0" + TStorlek + CHR(13)
    /*               cEan       = (IF EtikettLogg.Ean = "0" THEN "" ELSE EtikettLogg.Ean) + CHR(13) */
                  cEan       = "" + CHR(13)
                  cStorl = LEFT-TRIM(Tstorlek,"0")
                  cStorl = SUBSTR(cStorl,1,LENGTH(cStorl) - 1) + "." + SUBSTR(cStorl,LENGTH(cStorl))
                  cTxt       = STRING(EtikettLogg.vg) + " " + STRING(EtikettLogg.lopnr) + "/" + 
               (IF NUM-ENTRIES(cStorl,".") = 2 AND ENTRY(2,cStorl,".") = "0" THEN ENTRY(1,cStorl,".") ELSE cStorl) + CHR(13)
    /*               cPlu       = STRING(EtikettLogg.plunr) + CHR(13) */
                  cPlu       = "" + CHR(13)
                  cKr        = ENTRY(1,STRING(EtikettLogg.pris,"zzzz9.99")) + CHR(13)
                  cOren      = ENTRY(2,STRING(EtikettLogg.pris,">>>9.99")) + CHR(13)
                  cAntal     = '!P' + STRING(EtikettLogg.Ant) + CHR(13).
           iTeller = iTeller + EtikettLogg.Ant.

           PUT UNFORMATTED
           "^XA" SKIP
           "^FO1,5^BY2" SKIP
           "^B2N,60,N,N,N" SKIP
           "^FD" cInterleave "^FS" SKIP
           "^FO1,70" SKIP
           "^A0N,50,40" SKIP
           "^FD" cTxt "^FS" SKIP
           "^FO1,115" SKIP
           "^A0N,60,40" SKIP
           "^FD   Kr: " cKr "." cOren "^FS" SKIP
           "^PQ" STRING(EtikettLogg.Ant * 2) SKIP
           "^MCY" SKIP
           "^XZ" SKIP.
          END.
    end.
    OUTPUT CLOSE.
    EMPTY TEMP-TABLE etikettlogg.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-ChrConv) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ChrConv Procedure 
FUNCTION ChrConv RETURNS CHARACTER
  ( INPUT BongStr AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  I starten av pgm skall kontrolleras om konv skall göras
------------------------------------------------------------------------------*/
   DEF VAR iCount  AS INTEGER   NO-UNDO.
   DEF VAR wNewStr AS CHARACTER NO-UNDO.
/*    IF lIngenCharkonv = TRUE THEN */
/*        RETURN BongStr.           */
   DO iCount = 1 TO LENGTH(BongStr):
       ASSIGN wNewStr = wNewStr + CHR(ASC(SUBSTR(BongStr,iCount,1),"ibm850")).
   END.
   RETURN wNewStr.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

