&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*--------------------------------------------------------------------------
    File        : byggfilliste.w
    Purpose     : Bygge en liste med filnavn på filer i en filkatalog.
    Syntax      : run byggfilliste (input wFilKatalog, input wBuntType, output wErrNr).
    
                  ErrNr = 0  - Ok. Filiste er opprettet.
                          1  - FilListe er tom.
                          2  - Ukjent filkatalog gitt med som input.
                  I tillegg må temp-table tmpFilListe være definert i 
                  kallende program. Dette gjøres ved å inkludere:
                    
                    {tmpfilliste.i &New="New"}.

    Description : Mottar et filkatalognavn som input. 
                  Legger opp en liste over alle filene i en temp-table.
                  Sjekker mot FilLogg/BuntLogg om filen finnes fra før.
                  Finnes den, legges den ikke inn i temp-table.
                  
    Author(s)   : Tom Nøkleby
    Created     : 17/7-99
    Notes       : 
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* *********************** Parameter definisjoner ********************* */
/* Parameter Definisjoner ---                                           */
&IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
  def var wKatalog    as char           no-undo.
  DEF VAR wFilPrefix  AS CHAR           NO-UNDO.
  DEF VAR wFilExtent  AS CHAR           NO-UNDO.
  def var wErrNr      as int  initial 0 no-undo.
  assign
    wKatalog = "ht\ht-bht".
  {tmpfilliste.i &New="New"}
&ELSE
  def input  parameter wKatalog    as char           no-undo.
  DEF INPUT  PARAMETER wFilPrefix  AS CHAR           NO-UNDO.
  DEF INPUT  PARAMETER wFilExtent  AS CHAR           NO-UNDO.
  def output parameter wErrNr      as int  initial 0 no-undo.
  {tmpfilliste.i}
&ENDIF

/* ***************************  Definitions  ************************** */

def var wFilNavn    as char format "x(100)" no-undo.
def var wFullPath   as char format "x(200)" no-undo.
def var wEkstent    as char format "x(13)"  no-undo.
def var wAttributt  as char format "x(5)"  no-undo.

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
         HEIGHT             = 8.29
         WIDTH              = 40.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* Stripper uønskede karrakterer i slutten av katalognavnet */
if can-do("\,/",substring(wKatalog,length(wKatalog))) then
  wKatalog = substring(wKatalog,1,length(wKatalog) - 1).

def stream InnFil. /* Input stream for lesing av filer fra katalog */

form 
with frame Input-Frame width 250.

MAIN_BLOKK:
do on error undo, retry:
  /* Det oppsto feil ved åpning av fil. */
  if retry then
    do:
      wErrNr = 2.
      return "<avbryt>".
    end.

  /* Åpner stream til katalog */
  output to value("nul") no-echo.
  input stream InnFil from os-dir (wKatalog) no-echo.
  output close.

  /* Leser alle filer i katalogen */
  LES_FIL_NAVN:
  repeat:
    set stream InnFil 
      wFilNavn
      wFullPath
      wAttributt
    with frame Input-Frame. /* for å undertrykke display */
    
    /* Undertrykker allt annet enn filnavn */
    if index(wAttributt,"F") = 0 then
      next LES_FIL_NAVN.  
    
    /* Konverterer til små bokstaver. */  
    assign
      wFilNavn            = lc(wFilNavn)
      wFullPath           = lc(wFullPath)
      wEkstent            = IF num-entries(wFilNavn,".") > 1 then
                              entry(num-entries(wFilNavn,"."),wFilNavn,".")
                            ELSE ""
      FILE-INFO:FILE-NAME = wFullPath
      .
  
    /* Logger filen i listen hvis den ikke har vært lastet før dette året. */
    if not can-find(first tmpFilListe where
                      tmpFilListe.FilNavn = wFilNavn) or
       not can-find(first HT-FilHode where
                      HT-FilHode.FilNavn  = wFilNavn and
                      HT-FilHode.FilEkst  = wEkstent and
                      HT-FilHode.FilDato  = FILE-INFO:FILE-MOD-DATE and
                      HT-FilHode.FilTid   = FILE-INFO:FILE-MOD-TIME) then
    LOGG_FIL:
    do:

      /* Stemmer Extent? */
      IF wEkstent <> wFilExtent THEN
          LEAVE LOGG_FIL.
      /* Stemmer prefix? */
      IF NOT wFilnavn BEGINS wFilPrefix THEN
          LEAVE LOGG_Fil.

      create tmpFilListe.
      assign
        tmpFilListe.FilNavn   = wFilNavn
        tmpFilListe.FullPath  = wFullPath
        tmpFilListe.Ekstent   = wEkstent
        tmpFilListe.File-Size = (FILE-INFO:FILE-SIZE) 
        tmpFilListe.FilDato   = (FILE-INFO:FILE-MOD-DATE)
        tmpFilListe.FilTid    = (FILE-INFO:FILE-MOD-TIME).
    end. /* LOGG_FIL */
    
  end. /* LES_FIL_NAVN */

end. /* MAIN_BLOKK */

/* lukker input stream */
input stream InnFil close.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


