&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*------------------------------------------------------------------------
    File        : s9500.i 
    Purpose     : TempTable defineisjoner for innlesningsprogrammet.

    Syntax      :

    Description :

    Author(s)   : Tom Nøkleby
    Created     : 25/2-02
    Notes       : Benyttes av innlesningsprogrammer for HuginSveda kassene.
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF NEW SHARED TEMP-TABLE ldPost NO-UNDO
    FIELD recno        AS CHAR  FORMAT "X(6)" 
    FIELD logid        AS CHAR  FORMAT "X(3)" 
    FIELD tno          AS CHAR  FORMAT "X(3)" 
    FIELD oprno        AS CHAR  FORMAT "X(4)" 
    FIELD receipt      AS CHAR  FORMAT "X(7)" 
    FIELD transDate    AS CHAR  FORMAT "X(8)" 
    FIELD transTime    AS CHAR  FORMAT "X(6)" 
    FIELD n1           AS CHAR  FORMAT "X(20)"
    FIELD t1           AS CHAR  FORMAT "X(20)"
    FIELD n2           AS CHAR  FORMAT "X(3)" 
    FIELD n3           AS CHAR  FORMAT "X(6)"
    FIELD amount       AS DECIMAL
    FIELD amount2      AS DECIMAL
    FIELD quantity     AS DECIMAL
    FIELD t2           AS CHAR  FORMAT "X(7)" 
    FIELD tag          AS CHAR  FORMAT "X(1)"
    FIELD TTId         AS INT   FORMAT "999"
    FIELD TBId         AS INT   FORMAT "99"
    FIELD Tid          AS INT   
    FIELD OriginalData AS CHAR 
    INDEX tnoRec    IS PRIMARY tno receipt 
    INDEX Kvitto    tno receipt recno.

DEF NEW SHARED TEMP-TABLE ttKvittonr NO-UNDO
    FIELD tno        AS CHAR
    FIELD knr        AS CHAR
    FIELD kvitto_seq AS INTEGER
    INDEX tnoRec IS PRIMARY tno knr.

DEF NEW SHARED TEMP-TABLE ttStarReq NO-UNDO
    FIELD ean      AS CHAR
    FIELD nyttEan  AS CHAR
    FIELD kp       AS DECIMAL
    FIELD fp       AS DECIMAL
    FIELD mediakod AS CHAR
    FIELD datum    AS DATE
    INDEX ean IS PRIMARY ean.

/* TEMP-TABLE ttKvitto */.
{kvitto.i "new shared" "no-undo"}
/* TEMP-TABLE ttKvittoRad */
{kvittorad.i "new shared" "no-undo"}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Include
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: INCLUDE-ONLY
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Include ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Include 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


