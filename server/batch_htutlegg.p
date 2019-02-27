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

DEFINE TEMP-TABLE TT_ht-type NO-UNDO LIKE ht-type.
DEFINE VARIABLE TTh AS HANDLE      NO-UNDO.

DEF TEMP-TABLE TT_Art NO-UNDO
    FIELD Artikkelnr AS DEC.


/* DEF TEMP-TABLE PrisUt NO-UNDO                    */
/*     FIELD ButikkNr AS INT FORMAT ">>9"           */
/*     FIELD Ean      AS DEC FORMAT ">>>>>>>>>>>>9" */
/*     FIELD Varetekst AS CHAR                      */
/*     FIELD BestNr    AS INT                       */
/*     FIELD Tilbud    AS LOG                       */
/*     FIELD UtprisUt  AS DEC FORMAT ">>,>>9.99"    */
/*     FIELD UtprisInn AS DEC FORMAT ">>,>>9.99"    */
/*     INDEX Ean IS PRIMARY Ean.                    */

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

RUN INIT_TT.
IF CAN-FIND(FIRST TT_HT-Type WHERE TT_HT-Type.HTAktiv = TRUE) THEN 
DO:
    RUN Init_Artikkler.
    TTh = BUFFER TT_Art:HANDLE.
    FOR EACH TT_HT-Type WHERE TT_HT-Type.HTAktiv = TRUE:
        RUN VALUE(TT_HT-Type.eksportProg) (TTh ,
                                  TT_HT-Type.TypeId,
                                  TT_HT-Type.Eksportkatalog, /*  */
                                  TT_HT-Type.EkspFilPrefix,  /*  */
                                  TT_HT-Type.EkspFilEkstent                    /*  */
                                 ).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-Init_Artikkler) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Init_Artikkler Procedure 
PROCEDURE Init_Artikkler PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE dTst AS DECIMAL     NO-UNDO.
   FOR EACH ArtBas WHERE ArtBas.OPris = FALSE AND
                         ArtBas.Sanert = ? NO-LOCK:
       CREATE TT_Art.
       ASSIGN TT_Art.Artikkelnr = ArtBas.Artikkelnr NO-ERROR.
       IF ERROR-STATUS:ERROR THEN
           DELETE TT_Art.
   END.
/* 
DEF TEMP-TABLE TT_Art 
    FIELD ButikkNr AS INT FORMAT ">>9"
    FIELD Ean      AS DEC FORMAT ">>>>>>>>>>>>9"
    FIELD Varetekst AS CHAR 
    FIELD BestNr    AS INT  
    FIELD Tilbud    AS LOG FORMAT "Ja/Nei"
    FIELD UtTT_Art  AS DEC FORMAT ">>,>>9.99"
    FIELD UtprisInn AS DEC FORMAT ">>,>>9.99"
 */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-INIT_TT) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE INIT_TT Procedure 
PROCEDURE INIT_TT :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE cEksportprog AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFilExtent   AS CHARACTER   NO-UNDO.

FOR EACH ht-type WHERE htaktiv = TRUE NO-LOCK:
    cEksportprog = ENTRY(1,TRIM(HT-Type.eksportProg),".").
    cFilExtent   = TRIM(HT-Type.EkspFilEkstent).
    IF cEksportprog                 = "" OR
       TRIM(HT-Type.EkspFilPrefix)  = "" OR
       cFilExtent                   = "" OR
       TRIM(HT-Type.Eksportkatalog) = "" THEN
        NEXT.
    IF SEARCH(cEksportprog + ".p") = ? THEN
        NEXT.
    IF cFilExtent = "<ButNr>" THEN DO:
        cFilExtent = "".
        FOR EACH butiker WHERE Butiker.NedlagtDato = ? OR 
                               Butiker.NedlagtDato > TODAY NO-LOCK:
            cFilExtent = cFilExtent + (IF cFilExtent <> "" THEN "," ELSE "") + STRING(butiker.butik).
        END.
    END.
    CREATE TT_Ht-Type.
    ASSIGN TT_HT-Type.TypeId         = HT-Type.TypeId
           TT_HT-Type.Eksportkatalog = RIGHT-TRIM(TRIM(HT-Type.Eksportkatalog),"\") + "\"
           TT_HT-Type.EkspFilPrefix  = TRIM(HT-Type.EkspFilPrefix)
           TT_HT-Type.EkspFilEkstent = cFilExtent
           TT_HT-Type.eksportProg    = cEksportProg.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

