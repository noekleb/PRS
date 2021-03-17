/* fix-opprett_overforngsfil_fra_csv.p */

DEF VAR cInnFil AS CHAR NO-UNDO.
DEF VAR cUtFil AS CHAR NO-UNDO.
DEF VAR cKatalog AS CHAR NO-UNDO.
DEF VAR iLoop AS INT NO-UNDO.
DEF VAR cFelt AS CHAR EXTENT 42 NO-UNDO.
DEF VAR lDec AS DEC NO-UNDO.
DEF VAR iButNr AS INT NO-UNDO.

CURRENT-WINDOW:WIDTH = 350.

DEF STREAM Inn.
DEF STREAM Ut.

FUNCTION FixStorl RETURNS CHARACTER 
	( pcStorl AS CHAR ) FORWARD.

ASSIGN
    ibutNr   = 16
    cKatalog = 'konv\'
    cInnfil  = 'Lagerliste_but_16.csv'
    cUtfil   = 'OVERF'
    .

INPUT STREAM Inn FROM VALUE(cKatalog + cInnFil).
REPEAT:
    IMPORT STREAM Inn DELIMITER ';'
        cFelt.

    ASSIGN
        lDec = DEC(cFelt[2]) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
        NEXT.

    /* Korrigerer størrelsen */
    cFelt[6] = FixStorl(cFelt[6]).

    /* Henter størrelsestabell */
    FIND StrKonv NO-LOCK WHERE 
        StrKonv.Storl = cFelt[6] NO-ERROR.
    FIND FIRST ArtLag NO-LOCK WHERE 
        ArtLag.ArtikkelNr = DEC(cFelt[2]) AND 
        TRIM(ArtLag.Storl)      = TRIM(cFelt[6]) AND 
        ArtLag.Butik      = ibutNr NO-ERROR.

    IF NOT AVAILABLE StrKonv THEN
    DISPLAY
        cFelt[2] FORMAT "x(15)"
        cFelt[3] FORMAT "x(30)"
        cFelt[4] FORMAT "x(20)"
        cFelt[6] FORMAT "x(10)"
        (IF AVAILABLE StrKonv THEN STRINg(StrKonv.StrKode) ELSE '*Ukjent') COLUMN-LABEL 'Strkode'
        (IF AVAILABLE ArtLag THEN STRING(ArtLAg.StrKode) ELSE '*Ukjent') COLUMN-LABEL 'StrKode'
        cFelt[8] FORMAT "x(10)"
    WITH WIDTH 360.
END.
INPUT STREAM Inn CLOSE.

/* ------------- Funksjoner -------------------- */
FUNCTION FixStorl RETURNS CHARACTER 
	    ( pcStorl AS CHAR ):
/*------------------------------------------------------------------------------
		Purpose:  																	  
		Notes:  																	  
------------------------------------------------------------------------------*/	
 ASSIGN
    pcStorl = TRIM(pcStorl)
    pcStorl = CAPS(pcStorl)
    pcStorl = IF (LENGTH(pcStorl) = 1 OR 
                 LENGTH(pcStorl) = 3
                 ) 
                then " " + pcStorl
                else pcStorl.          

  /* Bytter ut eventuelle comma med punkt. */
  IF INDEX(pcStorl,",") <> 0 THEN
    OVERLAY(pcStorl, INDEX(pcStorl,","), 1, "CHARACTER") = ".".

  RETURN pcStorl.   /* Function return value. */
END FUNCTION.
