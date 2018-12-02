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
    Notes       : RUN asPakkseddel.p (BongLinje.ButikkNr, BongLinje.Antall, bEtikettKasse).
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE INPUT  PARAMETER B_Id            AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipButikkNr AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER dDatum    AS DATE        NO-UNDO.
DEFINE INPUT PARAMETER iKassaNr  AS INTEGER     NO-UNDO.
DEFINE INPUT PARAMETER iKvittoNr AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER lSkomodus       AS LOGICAL     NO-UNDO.
DEFINE OUTPUT PARAMETER iButikkNr       AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER dBongpris       AS DECIMAL     NO-UNDO.
DEFINE OUTPUT PARAMETER iBongNr         AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER iSelgernr       AS INTEGER     NO-UNDO.
DEFINE OUTPUT PARAMETER cMedlemskort    AS CHARACTER   NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER TABLE-HANDLE hTable.
DEFINE OUTPUT PARAMETER bOk             AS LOG NO-UNDO.
DEFINE OUTPUT PARAMETER cReturn         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cVgLop  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hBuffer AS HANDLE      NO-UNDO.

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

/* Liste over butikker som ikke skal ha etiketter når de gjør varemottak via kassen. */
/* Dette gjelder varemottak som gjøres ved å bestille etiketter fra pakkseddel.      */
/* Ref. løsning som er gjort for Gant.                                               */

IF B_Id <> "" THEN DO:
    IF B_Id BEGINS "KO" THEN DO:
        B_Id = SUBSTR(B_Id,3).
        FIND FIRST Bonghode WHERE BongHode.KOrdre_Id = DECI(B_Id) NO-LOCK NO-ERROR.
        IF AVAIL Bonghode THEN
            B_Id = STRING(BongHode.B_Id).
        ELSE
            B_Id = "-1".
    END.
    FIND Bonghode WHERE bonghode.b_id = DECI(B_Id) NO-LOCK NO-ERROR.
END.
ELSE
    FIND Bonghode WHERE bonghode.butikknr = ipButikkNr AND
                        bonghode.dato     = dDatum    AND
                        bonghode.gruppenr = 1         AND
                        bonghode.kassenr  = iKassaNr  AND
                        bonghode.bongnr   = iKvittoNr NO-LOCK NO-ERROR.

IF AVAIL Bonghode THEN DO:
    IF Bonghode.makulert = 2 THEN DO:
        bOK = FALSE.
        cReturn = "Makulerat kvitto".
    END.
    ELSE DO:
        IF CAN-FIND(FIRST bonglinje WHERE bonglinje.b_id = bonghode.b_id AND bonglinje.ttid = 1 AND bonglinje.makulert = FALSE) THEN DO:
            hBuffer = hTable:DEFAULT-BUFFER-HANDLE.
            /* bygg TT */
            iButikknr = bonghode.butikknr.
            dBongpris = bonghode.Belop.
            iBongNr   = bonghode.Bongnr.
            cMedlemskort = bonghode.Medlemskort.
/*             IF bonghode.medlemsnr > 0 THEN DO:                                            */
/*                 FIND medlem WHERE medlem.medlemsnr = bonghode.medlemsnr NO-LOCK NO-ERROR. */
/*                 IF AVAIL medlem THEN DO:                                                  */
/*                     IF LENGTH(TRIM(Medlem.PersonNr)) = 10 THEN                            */
/*                         cMedlemskort = TRIM(Medlem.PersonNr).                             */
/*                     ELSE DO:                                                              */
/*                         FIND FIRST medlemskort OF medlem NO-LOCK NO-ERROR.                */
/*                         IF AVAIL medlemskort THEN                                         */
/*                             cMedlemskort = medlemskort.kortnr.                            */
/*                     END.                                                                  */
/*                 END.                                                                      */
/*             END.                                                                          */
            FIND butikkselger WHERE butikkselger.butikknr = bonghode.butikknr AND butikkselger.selgernr = bonghode.selgernr NO-LOCK NO-ERROR.
                iSelgernr = IF AVAIL butikkselger THEN butikkselger.selgerid ELSE bonghode.selgernr.
            FOR EACH bonglinje WHERE bonglinje.b_id = bonghode.b_id NO-LOCK.
                IF NOT CAN-DO("1,10",STRING(Bonglinje.ttid)) THEN
                    NEXT.
                IF bonglinje.makulert = TRUE THEN
                    NEXT.
                cVgLop = bonglinje.artikkelnr.
                FIND artbas WHERE artbas.artikkelnr = DECI(bonglinje.artikkelnr) NO-LOCK NO-ERROR.
                IF NOT AVAIL artbas THEN
                    NEXT.
                IF lSkomodus AND artbas.vg <> 0 AND artbas.lopnr <> 0 THEN DO:
                    cVgLop = STRING(artbas.vg) + "/" + STRING(artbas.lopnr).
                END.
                hBuffer:BUFFER-CREATE().
                hBuffer:BUFFER-COPY(BUFFER bonglinje:HANDLE).
                hBuffer:BUFFER-FIELD("TBId"):BUFFER-VALUE = 0. 
                hBuffer:BUFFER-FIELD("OriginalData"):BUFFER-VALUE = cVgLop. 
                hBuffer:BUFFER-FIELD("Levkod"):BUFFER-VALUE = ArtBas.levkod. 
/*                 CREATE tt_BL.                            */
/*                 BUFFER-COPY bonglinje TO tt_BL NO-ERROR. */
/*                 IF ERROR-STATUS:ERROR THEN               */
/*                     DELETE tt_BL.                        */
            END.
            bOK = TRUE.
        END.
        ELSE DO:
            bOK = FALSE.
            cReturn = "Ingen försäljning".
        END.
    END.
END.
ELSE DO:
    bOK = FALSE.
    cReturn = "Okänt kvitto".
END.

/* Legger opp liste */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


