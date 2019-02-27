
/*------------------------------------------------------------------------
    File        : klarkjorElogg_run.p 
    Purpose     : 

    Syntax      :

    Description : Starter eksport

    Author(s)   : tomn
    Created     : Mon Apr  17:32:28 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/*ROUTINE-LEVEL ON ERROR UNDO, THROW.*/

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE ocReturn   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDatoTid   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lTid       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cVareFiler AS CHARACTER NO-UNDO.
DEFINE VARIABLE cMixFiler  AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAntVarer  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iAntPakker AS INTEGER   NO-UNDO.
DEFINE VARIABLE cOutputDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE cButikkLst AS CHARACTER NO-UNDO.

{syspara.i 1 1 56 cOutputDir}
cOutputDir = TRIM(RIGHT-TRIM(cOutputDir,'\')).

ASSIGN lTid = TIME.

RUN bibl_logg.p ('klargjor_elogg', 'klargjorElogg_run.p: WinCheduler Starter eksport av XML filer til katalog. ' + cOutputDir).

IF CAN-FIND(FIRST prisko WHERE PrisKo.AktiveresDato < TODAY) OR
   CAN-FIND(FIRST prisko WHERE PrisKo.AktiveresDato = TODAY AND 
                               PrisKo.AktiveresTid < TIME) THEN DO:
    RUN bibl_logg.p ('klargjor_elogg', 'klargjorElogg_run.p: Klargjør priskøen. ').
    RUN x-klargjorprisko.w (?).
    RUN bibl_logg.p ('klargjor_elogg', 'klargjorElogg_run.p: Priskø klargjort. ').
END.
ELSE RUN bibl_logg.p ('klargjor_elogg', 'klargjorElogg_run.p: Ingen priskøposter funnet. ').

/* Utlegg av xml filer */
IF CAN-FIND(FIRST ELogg WHERE ELogg.TabellNavn = "ArtPris" AND ELogg.EksterntSystem = "POS") THEN 
EKSPORT:
DO:
  RUN bibl_logg.p ('klargjor_elogg', 'klargjorElogg_run.p: Utlegg av XML filer til katalog. ' + cOutputDir).
  
  BUTIKKLOOP:
  FOR EACH Butiker NO-LOCK WHERE
    Butiker.FalckMedlNr = '1111':
    ASSIGN
        cButikkLst = cButikkLst + 
                     (IF cButikkLst = '' THEN '' ELSE ',') + 
                     STRING(Butiker.Butik).
  END. /* BUTIKKLOOP */
    
  ASSIGN
      cVareFiler = ''
      cMixFiler  = ''
      iAntVarer  = 0
      iAntPakker = 0
      .
    
  RUN bibl_logg.p ('klargjor_elogg', 'klargjorElogg_run.p: Utlegg til butikker: ' + cButikkLst).
  /* 2 Parameter efter cOutputdir ær en ingång vi skall anvænda før utlægg av kampanjer, Kommer nedan */
  RUN ArtBas2Nucleus.p (?,
                      INPUT cButikkLst,
                      INPUT cOutputDir,
                      0,
                      TRUE,
                      'POS',
                      OUTPUT cVareFiler,
                      OUTPUT cMixFiler,
                      OUTPUT iAntVarer,
                      OUTPUT iAntPakker).
                        
  IF cVarefiler <> '' THEN RUN bibl_logg.p ('klargjor_elogg', 'klargjorElogg_run.p: Varefiler: ' + cVarefiler).
  IF cMixFiler  <> '' THEN RUN bibl_logg.p ('klargjor_elogg', 'klargjorElogg_run.p: MixFiler: '  + cMixFiler).
  
  RUN bibl_logg.p ('klargjor_elogg', 'klargjorElogg_run.p: Utlegg av XML filer ferdig.').
  
END. /* EKSPORT */
/* Ingen poster å behandle. */
ELSE DO:
  RUN bibl_logg.p ('klargjor_elogg', 'klargjorElogg_run.p: WinCheduler Ingen ELoggsposter funnet. ').
END.

ASSIGN lTid = TIME - lTid.
RUN bibl_logg.p ('klargjor_elogg', 'klargjorElogg_run.p: WinCheduler Stoppet eksport av XML filer. ' + ' Tidsbruk: ' + string(lTid,"HH:MM:SS")).

QUIT.
