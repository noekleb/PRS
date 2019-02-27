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

DEF TEMP-TABLE tmpBong
    FIELD BongNr   AS INT
    FIELD Dato     AS DATE
    FIELD ButikkNr AS INT
    FIELD KundeNr  AS DEC
    .

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

FOR EACH KundeBetTrans WHERE
  KundeBetTRans.TTId = 63:

  DELETE KundeBetTrans.
END.

/* Skal ikke telles med. */
FOR EACH KundeBetTrans:
    IF CAN-DO("050,051,052,053,054,055,056,059,058,134,065,070,079",STRING(KundeBetTrans.TTId,"999")) THEN
        KundeBetTrans.Motpostert = true.
END.

FOR EACH KundeBetTrans WHERE
  KundeBetTRans.TTId >= 90 AND
  KundeBetTrans.TTId <= 99:

  DELETE KundeBetTrans.
END.


FOR EACH tmpBong:
    DELETE tmpBong.
END.
FOR EACH KundeTrans NO-LOCK:
    IF NOT CAN-FIND(FIRST tmpBong WHERE
                    tmpBong.BongNr = KundeTrans.BongId AND
                    tmpBong.BongNr <> 0) THEN
    DO:
        CREATE tmpBong.
        ASSIGN
            tmpBong.BongNr   = KundeTrans.BongId
            tmpBong.Dato     = KundeTrans.Dato
            tmpBong.ButikkNr = KundeTrans.butik
            tmpBong.KundeNr  = KundeTrans.KundeNr
            .
    END.
END.

/* Tar bort alle bonger som har kreditsalgslinje. */
FOR EACH tmpBong:
    IF CAN-FIND(FIRST BongLinje NO-LOCK WHERE
                    BongLinje.butikkNr = tmpBong.ButikkNr AND
                    BongLinje.Dato     = tmpBong.Dato and
                    BongLinje.BongNr   = tmpBong.BongNr AND
                    BongLinje.TTId     = 65) THEN
    DO:
        DELETE tmpBong.
    END.
END.

/* Motposterer alle poster som er betalt kontant eller med bank. */
FOR EACH tmpBong:
    FOR EACH KundeTrans WHERE
        KundeTrans.KundeNr = tmpBong.KundeNr AND
        KundeTrans.Butik   = tmpBong.ButikkNr AND
        KundeTrans.Dato    = tmpBong.Dato AND
        KundeTrans.BongId  = tmpBong.BongNr:
        ASSIGN
            KundeTrans.MotPostert = TRUE
            .

    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


