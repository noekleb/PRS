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
DEF VAR wStTypeListe AS CHAR NO-UNDO.

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

DEF VAR wLoop1      AS INT  NO-UNDO.
DEF VAR wDataObjekt AS CHAR NO-UNDO.
DEF VAR wWork1      AS DEC  NO-UNDO.
DEF VAR wWork2      AS DEC  NO-UNDO.
DEF VAR wWork3      AS DEC  NO-UNDO.

/* Liste over de StTypeId som det skal bygges StLAger for. */
ASSIGN
    wStTypeListe = "BUTSTAT,HOVEDGR,LEVERAN,LEVERAN-VG,VAREGR,AVDELING".

/* Renser den akkumulerte lagerfilen først. */
PUBLISH 'infoDisp' ("Tømmer StLager tabellen.").
FOR EACH StLager EXCLUSIVE-LOCK:
    DELETE StLAger.
END.

PUBLISH 'infoDisp' ("Bygger StLager tabellen.").

/* Leser alle lagerposter og akkumulerer disse opp i StLager. */
FOR EACH ArtBas NO-LOCK WHERE
    /*ArtBas.LopNr >= 1 AND
    ArtBas.LopNr <= 9999  AND */
    ArtBas.OPris = FALSE AND /* Ikke PLU artikkler. */
    ArtBas.Lager = TRUE     /* Ikke artikkler som ikke er lagerstyrt. */
    USE-INDEX ArtikkelNr:

 PUBLISH 'infoDisp' ("Artikkel: " + string(Artbas.ArtikkelNr) + " " + ArtBas.Beskr + " Vg: " + STRING(ArtBas.Vg) + ".").

  FOR EACH Lager OF ArtBas NO-LOCK:
      STLOOP:
      DO wLoop1 = 1 TO NUM-ENTRIES(wStTypeListe):

        /* Setter dataobjekt */
        case entry(wLoop1,wStTypeListe):
          when "AVDELING"  then
            do:
              FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
              IF NOT AVAIL VarGr THEN
                  NEXT STLOOP.
              find HuvGr OF VarGr no-lock NO-ERROR.
              IF NOT AVAIL HuvGr THEN
                  NEXT STLOOP.
              IF CAN-FIND(Avdeling OF HuvGr) THEN
                  wDataObjekt = string(HuvGr.AvdelingNr,"9999").
              ELSE NEXT STLOOP.
            end.
          when "HOVEDGR"  then
            do:
              FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
              IF NOT AVAIL VarGr THEN
                  NEXT STLOOP.
              IF CAN-FIND(HuvGr OF VarGr) THEN
                wDataObjekt = string(VarGr.Hg,"9999").
              ELSE NEXT STLOOP.
            end.
          when "VAREGR"   then
            wDataObjekt = string(ArtBas.Vg,"999999").
          when "LEVERAN"  then
            wDataObjekt = string(ArtBas.LevNr,"999999").
          when "BUTSTAT" then
            wDataObjekt = string(Lager.Butik,"999999").
          when "LEVERAN-VG"  then
            wDataObjekt = string(ArtBas.LevNr,"999999") + CHR(1) + 
                            string(ArtBas.Vg,"999999").
        end case.

        FIND StLager EXCLUSIVE-LOCK WHERE
            StLager.StTypeId   = ENTRY(wLoop1,wStTypeListe) AND
            StLager.DataObjekt = wDataObjekt AND
            StLager.Butik      = Lager.Butik NO-ERROR.
        IF NOT AVAILABLE StLAger THEN
        DO:
            CREATE StLager.
            ASSIGN
                StLager.StTypeId   = ENTRY(wLoop1,wStTypeListe) 
                StLager.DataObjekt = wDataObjekt 
                StLager.Butik      = Lager.Butik.
        END.
        
        /* Akkumulerer lager */
        {byggstlager.i "StLager" "Lager"}
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


