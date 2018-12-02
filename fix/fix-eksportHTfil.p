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

DEFINE VARIABLE TTh               AS HANDLE      NO-UNDO.
DEFINE VARIABLE iHTtype           AS INTEGER     NO-UNDO.
DEFINE VARIABLE cEksportKatalog   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEkspFilPrefix    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEksFilEkstent    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEksFilEkstentTmp AS CHARACTER   NO-UNDO.

ASSIGN 
TTh               = BUFFER Artbas:HANDLE
iHTtype           = 10
cEksportKatalog   = 'c:\home\lindbak\sendes'
cEkspFilPrefix    = 'Varer'
cEksFilEkstent    = '272'
cEksFilEkstentTmp = 'tmp'
.

DEFINE VARIABLE iLoop AS INTEGER     NO-UNDO.
DEFINE VARIABLE cFilNavn AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDir AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lVVarekost AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iCL        AS INTEGER   NO-UNDO.
DEFINE VARIABLE pcTekst    AS CHARACTER NO-UNDO.
DEFINE STREAM Ut.
DEFINE STREAM Ut2.

DEF TEMP-TABLE CpmUt NO-UNDO
    FIELD TextUt AS CHAR
    INDEX TextUt IS PRIMARY TextUt.

DEF TEMP-TABLE PrisUt NO-UNDO
    FIELD ButikkNr AS INT FORMAT "->>>>>9"
    FIELD Ean      AS DEC FORMAT "->>>>>>>>>>>>9"
    FIELD Varetekst AS CHAR 
    FIELD BestNr    AS INT  
    FIELD Tilbud    AS LOG
    FIELD UtprisUt  AS DEC FORMAT "->>>>>,>>9.99"
    FIELD UtprisInn AS DEC FORMAT "->>>>>,>>9.99"
    INDEX Ean IS PRIMARY Ean.
    
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

DEFINE TEMP-TABLE PrisCubUt
    /*  1 */ FIELD ArtikkelNr AS DECIMAL FORMAT ">>>>>>>>>>>>>9"
    /*  2 */ FIELD StrKode AS INTEGER FORMAT ">>>>9"
    /*  3 */ FIELD ModellFarge AS DECIMAL FORMAT ">>>>>>>>>>>>>9"
    /*  4 */ FIELD Bestillingsnr AS CHARACTER FORMAT "x(30)"
    /*  5 */ FIELD VgLopNr AS CHARACTER FORMAT "x(30)"
    /*  6 */ FIELD Varetekst AS CHARACTER FORMAT "x(30)" 
    /*  7 */ FIELD LevFargKod AS CHARACTER FORMAT "x(30)"
    /*  8 */ FIELD VgBeskr AS CHARACTER FORMAT "x(30)"
    /*  9 */ FIELD Vg AS INTEGER FORMAT ">>>>>9"
    /* 10 */ FIELD LopNr AS INTEGER FORMAT ">>>>>9"
    /* 11 */ FIELD Storl AS CHARACTER FORMAT "x(20)"
    /* 12 */ FIELD ButikkNr AS INTEGER FORMAT ">>>>>9"
    /* 13 */ FIELD PrisHK AS DECIMAL FORMAT "->>,>>>,>>9.99"
    /* 14 */ FIELD PrisLokal AS DECIMAL FORMAT "->>,>>>,>>9.99"
    /* 15 */ FIELD Varekost AS DECIMAL FORMAT "->>,>>>,>>9.99"
    /* 16 */ FIELD VektetVarekost AS DECIMAL FORMAT "->>,>>>,>>9.99"
    /* 17 */ FIELD Lagerstyrt AS LOG 
    /* 18 */ FIELD Strekkode AS CHARACTER FORMAT "x(30)"      
    /* 19 */ FIELD Interleave AS CHARACTER FORMAT "x(30)" 
    /* 20 */ FIELD MvaKode AS INTEGER FORMAT ">9"
    /* 21 */ FIELD LinkVare AS CHARACTER FORMAT "x(20)"
    /* 22 */ FIELD Antall AS INTEGER FORMAT ">>>9"
    /* 23 */ FIELD AntallBulk AS INTEGER FORMAT ">>>9"
    /*INDEX Strekkode Strekkode*/.

DEFINE BUFFER clButiker FOR Butiker.

DEFINE STREAM Ut.
DEFINE STREAM Inn.

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

FIND clButiker NO-LOCK WHERE
    clButiker.Butik = iCL NO-ERROR.
IF NOT AVAILABLE clButiker THEN
    RETURN.

IF iHTtype = 5 THEN DO:
    RUN ByggCpmvare.
    RUN Cpmvare_Ut.
END.
ELSE IF iHTtype = 6 THEN DO:
    RUN ByggPrisut.
    RUN Prisut_ut.
END.
ELSE IF iHTtype = 7 THEN 
DO:
    RUN ByggBxSentral.
    RUN PrisUtBxSentral.
END.
ELSE IF iHTtype = 10 THEN 
DO:
    RUN ByggBxMobileUt. 
    RUN eksportHTButikker.p(iHTtype,
                            cEksportKatalog,
                            'Butiker',
                            cEksFilEkstent
                            ).
END.
ELSE IF iHTtype = 12 THEN DO:
    RUN CubByggPrisUt.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ByggBxMobileUt) = 0 &THEN
    
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggBxMobileUt Procedure
PROCEDURE ByggBxMobileUt:
  /*------------------------------------------------------------------------------
      Purpose:                                      
      Notes:                                      
  ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cFil2           AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cStr            AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cLesFil         AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cSkrivFil       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE hQuery          AS HANDLE NO-UNDO.
    DEFINE VARIABLE hBuffer         AS HANDLE NO-UNDO.
    DEFINE VARIABLE dTst            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cStrekkode      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cKode           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iMvaKode        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cGTINFil        AS CHARACTER NO-UNDO.

    DEFINE BUFFER clArtPris FOR ArtPris.

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

    CREATE QUERY  hQuery.
    hQuery:SET-BUFFERS(TTh).
    hQuery:QUERY-PREPARE("for each " + TTh:NAME).
    hBuffer = hQuery:GET-BUFFER-HANDLE(1).
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST() NO-ERROR.
    
    LOOPEN:
    REPEAT WHILE NOT hQuery:QUERY-OFF-END:
        FIND ArtBas WHERE ArtBas.Artikkelnr = dec(hBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE()) NO-LOCK NO-ERROR.
        IF AVAIL Artbas AND ArtBas.ArtikkelNr > 0 AND 
         CAN-FIND(Lager WHERE Lager.ArtikkelNr = ArtBas.ArtikkelNr AND Lager.Butik = 272) THEN 
        ARTIKKEL:
        DO:
            FIND VarGr OF ArtBas NO-LOCK NO-ERROR.
            IF AVAILABLE VarGr THEN FIND Moms OF VarGr NO-LOCK NO-ERROR.
            IF AVAILABLE VarGr 
              THEN iMvaKode = VarGr.MomsKod.
              ELSE iMvaKode = 3.
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
            /*  5 */ iMvaKode ';'
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
        END. /* ARTIKKEL */
        hQuery:GET-NEXT() NO-ERROR.
    END. /* LOOPEN */

    OUTPUT STREAM Ut2 CLOSE.
    OUTPUT STREAM Ut CLOSE.
    
    DELETE OBJECT TTh NO-ERROR.
    DELETE OBJECT hQuery NO-ERROR.
    ASSIGN TTh = ?
           hQuery  = ?.
           
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


 
&IF DEFINED(EXCLUDE-ByggBxSentral) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggBxSentral Procedure
PROCEDURE ByggBxSentral:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    DEF VAR hQuery   AS HANDLE NO-UNDO.
    DEF VAR hBuffer  AS HANDLE NO-UNDO.
    DEFINE VARIABLE dTst AS DECIMAL     NO-UNDO.

    CREATE QUERY  hQuery.
    hQuery:SET-BUFFERS(TTh).
    hQuery:QUERY-PREPARE("for each " + TTh:NAME).
    hBuffer = hQuery:GET-BUFFER-HANDLE(1).
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST() NO-ERROR.
    REPEAT WHILE NOT hQuery:QUERY-OFF-END:
        FIND ArtBas WHERE ArtBas.Artikkelnr = dec(hBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE()) NO-LOCK NO-ERROR.
        IF AVAIL Artbas THEN DO:
            FIND FIRST artpris OF artbas NO-LOCK NO-ERROR.
            FOR EACH strekkode OF artbas NO-LOCK:
                IF LENGTH(TRIM(StrekKode.kode)) <> 13 THEN
                    NEXT.
                dTst = DECI(strekkode.kode) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    NEXT.
                    
                CREATE BxCentral.
                ASSIGN 
                    BxCentral.VareNr = TRIM(ArtBas.LevKod)
                    BxCentral.EAN = Strekkode.Kode
                    BxCentral.Beskrivelse = SUBSTRING(ArtBas.Beskr,1,40)
                    BxCentral.KjopsPris = STRING(ArtPris.Varekost[1])
                    BxCentral.MvaKode = '1'
                    BxCentral.BindingsVareNr = ''
                    BxCentral.Antall = '1'
                    BxCentral.AntallBulk = '1'
                    BxCentral.NettoPris = STRING(ArtPris.Pris[1])
                    NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    DELETE BxCentral.
    
            END.
        END.
        hQuery:GET-NEXT() NO-ERROR.
    END.
    DELETE OBJECT TTh NO-ERROR.
    DELETE OBJECT hQuery NO-ERROR.
    ASSIGN TTh = ?
           hQuery  = ?.


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF



&IF DEFINED(EXCLUDE-Prisut_ut) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prisut_ut Procedure 
PROCEDURE Prisut_ut :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE iButikkNr AS INTEGER     NO-UNDO.
DEFINE VARIABLE cFil2     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cStr      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLesFil   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSkrivFil AS CHARACTER   NO-UNDO.

ASSIGN iButikkNr = INT(ENTRY(1,cEksFilEkstent)).

IF NUM-ENTRIES(cEksFilEkstent) > 1 THEN
    cEksFilEkstentTmp = cEksFilEkstent.

cEksFilEkstent = STRING(iButikkNr) + "_TMP".
/* !!!!! */
ASSIGN
    cFilnavn = RIGHT-TRIM(RIGHT-TRIM(TRIM(cEksportKatalog), "\"),"/") + "\" +
                    cEkspFilPrefix + "." + cEksFilEkstent
    cDir = ENTRY(1,cFilnavn,"\") + "\"
    .
DO iLoop = 2 TO NUM-ENTRIES(cFilnavn,"\") - 1:
    cDir = cDir + ENTRY(iLoop,cFilnavn,"\") + "\".
    OS-CREATE-DIR VALUE(cDir).
END.

OUTPUT STREAM Ut TO VALUE(cFilnavn).
/* STATUS DEFAULT "Eksporterer til håndterminalfil...". */
/* Legger ut tingene sortert. */
    /* Legger ut tingene sortert. */
FOR EACH Prisut:
    PUT STREAM Ut UNFORMATTED
        iButikkNr " " 
        Prisut.Ean " "      
        '"' + Prisut.Varetekst + '"' " "
        Prisut.BestNr " "    
        Prisut.Tilbud " "   
        REPLACE(TRIM(STRING(Prisut.UtprisUt,"->>>>9.99")),",",".") " "
        REPLACE(TRIM(STRING(Prisut.UtprisInn,">>>>9.99")),",",".")
        SKIP
        .
END.
OUTPUT STREAM Ut CLOSE.

/* Nu har vi lagt ut _tmp till första butiken */
/* Om vi har flera butiker så skall filen kopieras / butiknr i filen ändras */
/* därefter så skall -tmp i filnamnet tas bort */
/* För att rename skall lyckas så måste vi först försäkra oss om att det inte ligger en fil där  */
/* sedan tidigare */
IF NUM-ENTRIES(cEksFilEkstentTmp) > 1 THEN DO: 
    cLesFil = cFilNavn.
    DO iLoop = 2 TO NUM-ENTRIES(cEksFilEkstentTmp):
        iButikkNr = INT(ENTRY(iLoop,cEksFilEkstentTmp)).
        cSkrivFil = ENTRY(1,cLesFil,".") + "." + STRING(iButikkNr) + "_TMP".
    END.
    INPUT FROM VALUE(cLesFil).
    OUTPUT TO VALUE(cSkrivFil).
    REPEAT:
        IMPORT UNFORMATTED cStr.
        IF TRIM(cStr) = "" THEN
            NEXT.
        ENTRY(1,cStr," ") = STRING(iButikkNr).
        PUT UNFORMATTED cStr SKIP.
    END.
    INPUT CLOSE.
    OUTPUT CLOSE.
    cFil2 = REPLACE(cSkrivFil,"_TMP","").
    OS-DELETE VALUE(cFil2).
    OS-RENAME VALUE(cSkrivFil) VALUE(cFil2).
END.

/* Sist så skall ursprungligat tmp-filen byta namn */
cFil2 = REPLACE(cFilnavn,"_TMP","").
OS-DELETE VALUE(cFil2).
OS-RENAME VALUE(cFilnavn) VALUE(cFil2).

END PROCEDURE.
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF
&IF DEFINED(EXCLUDE-ByggCpmvare) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggCpmvare Procedure 
PROCEDURE ByggCpmvare :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-----------------m-------------------------------------------------------------*/
    DEF VAR hQuery   AS HANDLE NO-UNDO.
    DEF VAR hBuffer  AS HANDLE NO-UNDO.
    DEFINE VARIABLE cBeskr AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE dTst AS DECIMAL     NO-UNDO.

    CREATE QUERY  hQuery.
    hQuery:SET-BUFFERS(TTh).
    hQuery:QUERY-PREPARE("for each " + TTh:NAME).
    hBuffer = hQuery:GET-BUFFER-HANDLE(1).
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST() NO-ERROR.
    REPEAT WHILE NOT hQuery:QUERY-OFF-END:
        FIND ArtBas WHERE ArtBas.Artikkelnr = dec(hBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE()) NO-LOCK NO-ERROR.
        IF AVAIL Artbas THEN DO:
            ASSIGN cBeskr  = TRIM(SUBSTRING(REPLACE(REPLACE(ArtBas.Beskr,",","."),'"',' '),1,20))
                   cBeskr  = (IF cBeskr = "" THEN "Blank varetekst" ELSE cBeskr)
                   cBeskr  = SUBSTRING(cBeskr,1,20)
                   cBeskr  = cBeskr + FILL(" ",20 - length(cBeskr)).
            FOR EACH strekkode OF artbas NO-LOCK:
                IF LENGTH(TRIM(StrekKode.kode)) <> 13 THEN
                    NEXT.
                dTst = DECI(strekkode.kode) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    NEXT.
                CREATE CpmUt.
                ASSIGN CpmUt.TextUt    = StrekKode.Kode + cBeskr NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    DELETE CpmUt.
            END.
        END.
        hQuery:GET-NEXT() NO-ERROR.
    END.
    DELETE OBJECT TTh NO-ERROR.
    DELETE OBJECT hQuery NO-ERROR.
    ASSIGN TTh = ?
           hQuery  = ?.



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ByggPrisut) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ByggPrisut Procedure 
PROCEDURE ByggPrisut :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR hQuery   AS HANDLE NO-UNDO.
    DEF VAR hBuffer  AS HANDLE NO-UNDO.
    DEFINE VARIABLE dTst AS DECIMAL     NO-UNDO.

    CREATE QUERY  hQuery.
    hQuery:SET-BUFFERS(TTh).
    hQuery:QUERY-PREPARE("for each " + TTh:NAME).
    hBuffer = hQuery:GET-BUFFER-HANDLE(1).
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST() NO-ERROR.
    REPEAT WHILE NOT hQuery:QUERY-OFF-END:
        FIND ArtBas WHERE ArtBas.Artikkelnr = dec(hBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE()) NO-LOCK NO-ERROR.
        IF AVAIL Artbas THEN DO:
            FIND FIRST artpris OF artbas NO-LOCK NO-ERROR.
            FOR EACH strekkode OF artbas NO-LOCK:
                IF LENGTH(TRIM(StrekKode.kode)) <> 13 THEN
                    NEXT.
                dTst = DECI(strekkode.kode) NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    NEXT.
                CREATE PrisUt.
                ASSIGN PrisUt.ButikkNr  = 0
                       PrisUt.Ean       = DECI(StrekKode.Kode)
                       PrisUt.Varetekst = SUBSTR(REPLACE(TRIM(ArtBas.Beskr),'"',''),1,20)
                       PrisUt.BestNr    = ?
                       PrisUt.Tilbud    = IF AVAIL artpris THEN artpris.tilbud ELSE FALSE
                       PrisUt.UtprisUt  = IF AVAIL artpris THEN Artpris.pris[IF artpris.tilbud THEN 2 ELSE 1]
                                               ELSE 0
                       PrisUt.UtprisInn = 0 NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                    DELETE PrisUt.
    
            END.
        END.
        hQuery:GET-NEXT() NO-ERROR.
    END.
    DELETE OBJECT TTh NO-ERROR.
    DELETE OBJECT hQuery NO-ERROR.
    ASSIGN TTh = ?
           hQuery  = ?.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-Cpmvare_Ut) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cpmvare_Ut Procedure 
PROCEDURE Cpmvare_Ut :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    ASSIGN 
        cFilnavn = RIGHT-TRIM(RIGHT-TRIM(TRIM(cEksportKatalog), "\"),"/") + "\" + 
                        cEkspFilPrefix + "." + cEksFilEkstent
        cDir = ENTRY(1,cFilnavn,"\") + "\"
        .
    DO iLoop = 2 TO NUM-ENTRIES(cFilnavn,"\") - 1:
        cDir = cDir + ENTRY(iLoop,cFilnavn,"\") + "\".
        OS-CREATE-DIR VALUE(cDir).
    END.
    OUTPUT STREAM Ut TO VALUE(cFilnavn).
    FOR EACH CpmUt:
        PUT STREAM Ut UNFORMATTED CpmUt.Textut SKIP.
    END.
    OUTPUT STREAM Ut CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CubByggPrisUt) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CubByggPrisUt Procedure
PROCEDURE CubByggPrisUt:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
    DEFINE VARIABLE iButikkNr       AS INTEGER     NO-UNDO.  
    DEFINE VARIABLE cFil2           AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cStr            AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cLesFil         AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cSkrivFil       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE hQuery          AS HANDLE NO-UNDO.
    DEFINE VARIABLE hBuffer         AS HANDLE NO-UNDO.
    DEFINE VARIABLE dTst            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cStrekkode      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE c2av5Interleave AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cKode           AS CHARACTER NO-UNDO.

    DEFINE BUFFER clArtPris FOR ArtPris.

    ASSIGN 
        iButikkNr = INT(ENTRY(1,cEksFilEkstent)).
    
    IF NUM-ENTRIES(cEksFilEkstent) > 1 THEN
        cEksFilEkstentTmp = cEksFilEkstent.

    cEksFilEkstent = STRING(iButikkNr) + "_TMP".
    ASSIGN
        cFilnavn = RIGHT-TRIM(RIGHT-TRIM(TRIM(cEksportKatalog), "\"),"/") + "\" +
                        cEkspFilPrefix + "." + cEksFilEkstent
        cDir = ENTRY(1,cFilnavn,"\") + "\".
    
    DO iLoop = 2 TO NUM-ENTRIES(cFilnavn,"\") - 1:
        cDir = cDir + ENTRY(iLoop,cFilnavn,"\") + "\".
        OS-CREATE-DIR VALUE(cDir).
    END.
    
    OUTPUT STREAM Ut TO VALUE(cFilnavn).

    CREATE QUERY  hQuery.
    hQuery:SET-BUFFERS(TTh).
    hQuery:QUERY-PREPARE("for each " + TTh:NAME).
    hBuffer = hQuery:GET-BUFFER-HANDLE(1).
    hQuery:QUERY-OPEN().
    hQuery:GET-FIRST() NO-ERROR.
    
    LOOPEN:
    REPEAT WHILE NOT hQuery:QUERY-OFF-END:
        FIND ArtBas WHERE ArtBas.Artikkelnr = dec(hBuffer:BUFFER-FIELD("Artikkelnr"):BUFFER-VALUE()) NO-LOCK NO-ERROR.
        IF AVAIL Artbas AND ArtBas.ArtikkelNr > 0 THEN 
        ARTIKKEL:
        DO:
            STREKKODE_BLOKK:
            FOR EACH strekkode OF artbas NO-LOCK WHERE 
                  TRIM(Strekkode.Kode) > '' 
                  BREAK BY Strekkode.ArtikkelNr
                        BY Strekkode.StrKode:
            
                IF LENGTH(TRIM(StrekKode.kode)) <> 13 THEN
                    NEXT.
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
                                    cKode
                  c2Av5Interleave = c2Av5Interleave + 
                                   (IF c2Av5Interleave = '' THEN '' ELSE ',') + 
                                    Strekkode.BestillingsNummer.
                /* Legger også ut 2AV5INTERLEAVE i strekkode feltet. */
                IF LENGTH(TRIM(Strekkode.Bestillingsnummer)) = 12 THEN
                INTERLEAVE: 
                DO:
                    dTst = DECI(Strekkode.Bestillingsnummer) NO-ERROR.
                    IF ERROR-STATUS:ERROR THEN
                      LEAVE INTERLEAVE.
                    ASSIGN
                      cStrekkode = cStrekkode + 
                                   (IF cStrekkode = '' THEN '' ELSE ',') + 
                                   Strekkode.Bestillingsnummer.
                END. /* INTERLEAVE */

                IF LAST-OF(Strekkode.StrKode) THEN
                STREKKODE: 
                DO:
                    FIND clArtPris NO-LOCK WHERE
                         clArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                         clArtPris.ProfilNr   = clButiker.ProfilNr NO-ERROR.
                    IF NOT AVAILABLE clArtPris THEN 
                      FIND FIRST clArtPris NO-LOCK WHERE
                          clArtPris.ArtikkelNr = ArtBas.ArtikkelNr NO-ERROR.
                    FIND StrKonv NO-LOCK WHERE 
                        StrKonv.StrKode = Strekkode.StrKode NO-ERROR.
                    FIND VarGr NO-LOCK OF ArtBas NO-ERROR.
                        
                    FIND butiker NO-LOCK WHERE
                        butiker.Butik = iButikkNr NO-ERROR.
                    IF AVAILABLE butiker THEN
                        FIND ArtPris NO-LOCK WHERE
                        ArtPris.ArtikkelNr = ArtBas.ArtikkelNr AND
                        ArtPris.ProfilNr   = Butiker.ProfilNr NO-ERROR.
                    IF NOT AVAILABLE ArtPris THEN
                        FIND FIRST ArtPris OF ArtBas NO-ERROR.
                        
                    /* Vektet varekost - Kun for lagerstyrte varer. */
                    IF AVAILABLE Lager THEN RELEASE Lager.
                    IF ArtBas.Lager THEN
                    DO:
                        FIND Lager NO-LOCK WHERE
                            Lager.ArtikkelNr = ArtBas.ArtikkelNr AND
                            Lager.Butik      = INT(ENTRY(1,cEksFilEkstent)) NO-ERROR.
                        IF (AVAILABLE Lager AND Lager.VVareKost <> ? AND Lager.VVarekost > 0) 
                            THEN lVVarekost = Lager.VVareKost.
                            ELSE lVVareKost = (IF AVAILABLE ArtPris THEN ArtPris.VareKost[1] ELSE 0).
                    END.
                    ELSE lVVarekost = (IF AVAILABLE ArtPris THEN ArtPris.Varekost[1] ELSE 0).    
                    
                    pcTekst = TRIM(REPLACE(ArtBas.Beskr,'"','')).
                    pcTekst = (IF pcTekst = "" THEN "Blank varetekst" ELSE pcTekst).

                    IF TRIM(cStrekkode) <> '' THEN 
                    DO:
                            PUT STREAM Ut UNFORMATTED
                        /*  1 */ ArtBas.ArtikkelNr  ';' 
                        /*  2 */ (IF AVAILABLE Strekkode THEN Strekkode.StrKode ELSE 0)  ';' 
                        /*  3 */ ArtBas.ModellFarge  ';'
                        /*  4 */ TRIM((IF Strekkode.Bestillingsnummer <> '' THEN Strekkode.Bestillingsnummer ELSE ArtBas.LevKod)) ';' 
                            /*  5 */ STRING(ArtBas.Vg) + '/' + (IF ArtBas.LopNr <> ? THEN STRING(ArtBas.LopNr) ELSE '0') ';'
                        /*  6 */ TRIM(REPLACE(REPLACE(REPLACE(TRIM(ArtBas.Beskr),';',','),CHR(10),''),CHR(13),'')) ';'
                        /*  7 */ TRIM(REPLACE(REPLACE(REPLACE(TRIM(ArtBas.LevFargKod),';',','),CHR(10),''),CHR(13),'')) ';'
                        /*  8 */ TRIM(REPLACE(REPLACE(REPLACE(TRIM(IF AVAILABLE VarGr THEN VarGr.VgBeskr ELSE ''),';',','),CHR(10),''),CHR(13),'')) ';'
                        /*  9 */ ArtBas.Vg  ';'
                        /* 10 */ (IF ArtBas.LopNr <> ? THEN ArtBas.LopNr ELSE 0) ';'
                        /* 11 */ (IF AVAILABLE StrKonv THEN REPLACE(TRIM(StrKonv.Storl),';',',') ELSE '') ';'
                        /* 12 */ ENTRY(1,cEksFilEkstent,'_') /* ButikkNr */ ';'
                        /* 13 */ REPLACE(TRIM(STRING(IF AVAILABLE clArtPris THEN clArtPris.Pris[1] ELSE 0,"->>>>>>9.99")),",",".")  ';'         
                        /* 14 */ REPLACE(TRIM(STRING(IF AVAILABLE ArtPris THEN ArtPris.Pris[1] ELSE 0,"->>>>>>9.99")),",",".")  ';'       
                        /* 15 */ REPLACE(TRIM(STRING(IF AVAILABLE ArtPris THEN ArtPris.Varekost[1] ELSE 0,"->>>>>>9.99")),",",".")  ';'       
                        /* 16 */ REPLACE(TRIM(STRING(lVVareKost,"->>>>>>9.99")),",",".")  ';' 
                        /* 17 */ ArtBas.Lager ';'
                        /* 18 */ cStrekkode ';'      
                        /* 19 */ c2Av5Interleave 
                            SKIP.
                    END.
                    ASSIGN 
                    cStrekkode      = ''
                    c2Av5Interleave = ''.
                END. /* STREKKODE */
            END. /* STREKKODE_BLOKK */
        END. /* ARTIKKEL */
        hQuery:GET-NEXT() NO-ERROR.
    END. /* LOOPEN */

    OUTPUT STREAM Ut CLOSE.
    
    DELETE OBJECT TTh NO-ERROR.
    DELETE OBJECT hQuery NO-ERROR.
    ASSIGN TTh = ?
           hQuery  = ?.
           
    /* Nu har vi lagt ut _tmp till första butiken */
    /* Om vi har flera butiker så skall filen kopieras / butiknr i filen ändras */
    /* därefter så skall -tmp i filnamnet tas bort */
    /* För att rename skall lyckas så måste vi först försäkra oss om att det inte ligger en fil där  */
    /* sedan tidigare */
    IF NUM-ENTRIES(cEksFilEkstentTmp) > 1 THEN 
    DO: 
        cLesFil = cFilNavn.
        DO iLoop = 2 TO NUM-ENTRIES(cEksFilEkstentTmp):
            iButikkNr = INT(ENTRY(iLoop,cEksFilEkstentTmp)).
            cSkrivFil = ENTRY(1,cLesFil,".") + "." + STRING(iButikkNr) + "_TMP".
        END.
        INPUT FROM VALUE(cLesFil).
        OUTPUT TO VALUE(cSkrivFil).
        REPEAT:
            IMPORT UNFORMATTED cStr.
            IF TRIM(cStr) = "" THEN
                NEXT.
            ENTRY(1,cStr," ") = STRING(iButikkNr).
            PUT UNFORMATTED cStr SKIP.
        END.
        INPUT CLOSE.
        OUTPUT CLOSE.
        cFil2 = REPLACE(cSkrivFil,"_TMP","").
        OS-DELETE VALUE(cFil2).
        OS-RENAME VALUE(cSkrivFil) VALUE(cFil2).
    END.
    
    /* sist så skall ursprungligat tmp-filen byta namn */
    cFil2 = REPLACE(cFilnavn,"_TMP","").
    OS-DELETE VALUE(cFil2).
    OS-RENAME VALUE(cFilnavn) VALUE(cFil2).
           
END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


 
&IF DEFINED(EXCLUDE-PrisUtBxSentral) = 0 &THEN
		
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE PrisUtBxSentral Procedure
PROCEDURE PrisUtBxSentral:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
DEFINE VARIABLE iButikkNr AS INTEGER     NO-UNDO.
DEFINE VARIABLE cFil2     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cStr      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLesFil   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSkrivFil AS CHARACTER   NO-UNDO.

ASSIGN iButikkNr = INT(ENTRY(1,cEksFilEkstent)).

IF NUM-ENTRIES(cEksFilEkstent) > 1 THEN
    cEksFilEkstentTmp = cEksFilEkstent.

cEksFilEkstent = STRING(iButikkNr) + "_TMP".
/* !!!!! */
ASSIGN
    cFilnavn = RIGHT-TRIM(RIGHT-TRIM(TRIM(cEksportKatalog), "\"),"/") + "\" +
                    cEkspFilPrefix + "." + cEksFilEkstent
    cDir = ENTRY(1,cFilnavn,"\") + "\"
    .
DO iLoop = 2 TO NUM-ENTRIES(cFilnavn,"\") - 1:
    cDir = cDir + ENTRY(iLoop,cFilnavn,"\") + "\".
    OS-CREATE-DIR VALUE(cDir).
END.

OUTPUT STREAM Ut TO VALUE(cFilnavn).
/* STATUS DEFAULT "Eksporterer til håndterminalfil...". */
/* Legger ut tingene sortert. */
    /* Legger ut tingene sortert. */
FOR EACH BxCentral:
    PUT STREAM Ut UNFORMATTED
        BxCentral.VareNr ';'
        BxCentral.EAN ';'
        '"' + BxCentral.Beskrivelse + '"' ';'
        BxCentral.KjopsPris ';'
        BxCentral.MvaKode ';'
        BxCentral.BindingsVareNr ';'
        BxCentral.Antall ';'
        BxCentral.AntallBulk ';'
        BxCentral.NettoPris
        SKIP.
END.
OUTPUT STREAM Ut CLOSE.

/* Nu har vi lagt ut _tmp till första butiken */
/* Om vi har flera butiker så skall filen kopieras / butiknr i filen ändras */
/* därefter så skall -tmp i filnamnet tas bort */
/* För att rename skall lyckas så måste vi först försäkra oss om att det inte ligger en fil där  */
/* sedan tidigare */
IF NUM-ENTRIES(cEksFilEkstentTmp) > 1 THEN DO: 
    cLesFil = cFilNavn.
    DO iLoop = 2 TO NUM-ENTRIES(cEksFilEkstentTmp):
        iButikkNr = INT(ENTRY(iLoop,cEksFilEkstentTmp)).
        cSkrivFil = ENTRY(1,cLesFil,".") + "." + STRING(iButikkNr) + "_TMP".
    END.
    INPUT FROM VALUE(cLesFil).
    OUTPUT TO VALUE(cSkrivFil).
    REPEAT:
        IMPORT UNFORMATTED cStr.
        IF TRIM(cStr) = "" THEN
            NEXT.
        ENTRY(1,cStr," ") = STRING(iButikkNr).
        PUT UNFORMATTED cStr SKIP.
    END.
    INPUT CLOSE.
    OUTPUT CLOSE.
    cFil2 = REPLACE(cSkrivFil,"_TMP","").
    OS-DELETE VALUE(cFil2).
    OS-RENAME VALUE(cSkrivFil) VALUE(cFil2).
END.

/* Sist så skall ursprungligat tmp-filen byta namn */
cFil2 = REPLACE(cFilnavn,"_TMP","").
OS-DELETE VALUE(cFil2).
OS-RENAME VALUE(cFilnavn) VALUE(cFil2).

END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


