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

DEFINE VARIABLE iHTtype           AS INTEGER     NO-UNDO.
DEFINE VARIABLE cEksportKatalog   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEkspFilPrefix    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEksFilEkstent    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEksFilEkstentTmp AS CHARACTER   NO-UNDO.
DEF VAR iCl AS INT NO-UNDO.
DEF VAR ibutNr AS INT NO-UNDO.

DEF BUFFER clArtPris FOR ArtPris.
DEF BUFFER clButiker FOR Butiker.

DEFINE TEMP-TABLE BxCentral NO-UNDO 
    FIELD VareNr AS CHARACTER FORMAT "x(40)"
    FIELD EAN AS CHARACTER FORMAT "x(40)"
    FIELD Beskrivelse AS CHARACTER FORMAT "x(100)"
    FIELD KjopsPris AS CHARACTER FORMAT "x(12)"
    FIELD MvaKode AS CHARACTER FORMAT "x(1)"
    FIELD BindingsVareNr AS CHARACTER FORMAT "x(20)"
    FIELD Antall AS CHARACTER FORMAT "x(10)"
    FIELD AntallBulk AS CHARACTER FORMAT "x(10)"
    FIELD NettoPris AS CHARACTER FORMAT "x(12)"
    .

DEFINE STREAM Ut.
DEFINE STREAM Ut2.

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

{syspara.i 5 1 1 iCl INT}

ASSIGN 
    iButNr            = 272
    iHTtype           = 1
    cEksportKatalog   = 'c:\home\lindbak\sendes'
    cEkspFilPrefix    = 'Varer'
    cEksFilEkstent    = STRING(ibutNr)
    cEksFilEkstentTmp = 'tmp'
    .


FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE clButiker THEN
    RETURN.

RUN eksporterVarefil.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-eksporterVarefil) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE eksporterVarefil Procedure 
PROCEDURE eksporterVarefil :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEF VAR lVVarekost AS DEC  NO-UNDO.
DEF VAR pcTekst    AS CHAR NO-UNDO.                 
DEF VAR dTst       AS DEC  NO-UNDO.
DEF VAR cKode      AS CHAR NO-UNDO.
DEF VAR cStrekkode AS CHAR NO-UNDO.
DEF VAR cFilnavn   AS CHAR NO-UNDO.
DEF VAR cGTINFil   AS CHAR NO-UNDO.
DEF VAR cDir       AS CHAR NO-UNDO.
DEF VAR iLoop      AS INT  NO-UNDO.
DEF VAR cFil2      AS CHAR NO-UNDO.

ASSIGN
    cFilnavn = RIGHT-TRIM(RIGHT-TRIM(TRIM(cEksportKatalog), "\"),"/") + "\" +
                    cEkspFilPrefix + "." + ENTRY(1,cEksFilEkstent,',') + "_TMP"
    cGTINFil = REPLACE(cFilnavn,'Varer','GTIN')
    cDir = ENTRY(1,cFilnavn,"\") + "\".

DO iLoop = 2 TO NUM-ENTRIES(cFilnavn,"\") - 1:
    cDir = cDir + ENTRY(iLoop,cFilnavn,"\") + "\".
    OS-CREATE-DIR VALUE(cDir).
END.

OUTPUT STREAM Ut TO VALUE(cFilnavn).
OUTPUT STREAM Ut2 TO VALUE(cGTINFil).

ARTLOOP:    
FOR EACH ArtBas NO-LOCK WHERE 
    ArtBas.ArtikkelNr > 0 AND 
    CAN-FIND(FIRST Lager WHERE 
             Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
             Lager.Butik      = iButNr):

    FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
    IF AVAILABLE VarGr THEN FIND Moms OF VarGr NO-LOCK NO-ERROR.
    FIND clArtPris NO-LOCK WHERE
         clArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
         clArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
    IF NOT AVAILABLE clArtPris THEN 
      FIND FIRST clArtPris NO-LOCK WHERE
          clArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
    FIND StrKonv NO-LOCK WHERE 
        StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
    FIND VarGr NO-LOCK OF ArtBas NO-ERROR.

    /* Vektet varekost - Kun for lagerstyrte varer. */
    IF AVAILABLE Lager THEN RELEASE Lager.
    IF ArtBas.Lager THEN
    DO:
        FIND Lager NO-LOCK WHERE
            Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
            Lager.Butik      = INT(ENTRY(1,cEksFilEkstent)) NO-ERROR.
        IF (AVAILABLE Lager AND Lager.VVareKost <> ? AND Lager.VVarekost > 0) 
            THEN lVVarekost = Lager.VVareKost.
            ELSE lVVareKost = (IF AVAILABLE clArtPris THEN clArtPris.VareKost[1] ELSE 0).
    END.
    ELSE lVVarekost = (IF AVAILABLE clArtPris THEN clArtPris.Varekost[1] ELSE 0).    

    pcTekst = TRIM(REPLACE(ArtBas.Beskr,'"','')).
    pcTekst = (IF pcTekst = "" THEN "Blank varetekst" ELSE pcTekst).

    PUT STREAM Ut UNFORMATTED
    /*  1 */ ArtBas.ArtikkelNr ';' 
    /*  2 */ ';' 
    /*  3 */ SUBSTRING(TRIM(REPLACE(REPLACE(REPLACE(TRIM(ArtBas.Beskr),';',','),CHR(10),''),CHR(13),'')),1,60) ';'
    /*  4 */ REPLACE(TRIM(STRING(lVVareKost,"->>>>>>9.99")),",",".")  ';' 
    /*  5 */ '0;'
    /*  6 */ ';'
    /*  7 */ '1;'
    /*  8 */ '1;'
    /*  9 */ REPLACE(TRIM(STRING(IF AVAILABLE clArtPris THEN clArtPris.Pris[1] ELSE 0,"->>>>>>9.99")),",",".")       
    SKIP.

    STREKKODE_BLOKK:
    FOR EACH strekkode OF artbas NO-LOCK WHERE 
          TRIM(Strekkode.Kode) > '' 
          BREAK BY Strekkode.ArtikkelNr
                BY Strekkode.StrKode:

        FIND StrKonv NO-LOCK WHERE 
             StrKonv.StrKode = Strekkode.StrKode NO-ERROR.

        dTst = DECI(strekkode.kode) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN
            NEXT.

        /* Trimmer bort ledende nuller i EAN8 koder. */
        cKode = Strekkode.Kode.
        IF cKode BEGINS '00000' THEN 
          cKode = SUBSTRING(cKode,6).  
        ASSIGN
          cStrekkode      = cStrekkode + 
                            (IF cStrekkode = '' THEN '' ELSE ',') + 
                            cKode.

        IF TRIM(cStrekkode) <> '' THEN 
        DO:
                PUT STREAM Ut2 UNFORMATTED
            /*  1 */ ArtBas.ArtikkelNr ';' 
            /*  2 */ (IF AVAILABLE Strekkode THEN Strekkode.Kode ELSE '') ';' 
            /*  3 */ (IF AVAILABLE StrKonv THEN TRIM(StrKonv.Storl) ELSE '')        
                SKIP.
        END.
        ASSIGN 
        cStrekkode      = ''.
    END. /* STREKKODE_BLOKK */

END. /* ARTLOOP */

OUTPUT STREAM Ut2 CLOSE.
OUTPUT STREAM Ut CLOSE.

/* sist så skall ursprungligat tmp-filen byta namn */
cFil2 = REPLACE(cFilnavn,"_TMP","").
OS-DELETE VALUE(cFil2).
OS-RENAME VALUE(cFilnavn) VALUE(cFil2).

/* sist så skall ursprungligat tmp-filen byta namn */
cFil2 = REPLACE(cGTINFil,"_TMP","").
OS-DELETE VALUE(cFil2).
OS-RENAME VALUE(cGTINFil) VALUE(cFil2).





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

