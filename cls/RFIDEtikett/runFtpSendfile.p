
/*------------------------------------------------------------------------
    File        : testRFIDEtikett.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed May 16 09:06:30 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTekst AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrefix             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEkstent            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKatalog            AS CHARACTER NO-UNDO.
DEFINE VARIABLE cKatalogLst         AS CHARACTER NO-UNDO.
DEFINE VARIABLE bOk                 AS LOG       NO-UNDO.
DEFINE VARIABLE cReturn             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTime               AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTimeLst            AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest               AS LOG       NO-UNDO. 
DEFINE VARIABLE iLoop               AS INTEGER NO-UNDO.

DEFINE VARIABLE rStandardFunksjoner AS CLASS cls.StdFunk.StandardFunksjoner NO-UNDO.
DEFINE VARIABLE rftpSendFile        AS CLASS cls.RFIDEtikett.ftpSendFile    NO-UNDO.

{ cls\StdFunk\filliste.i }

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN 
    cLogg       = 'ftpSendFile' + REPLACE(STRING(TODAY),'/','')
    cKatalogLst = ''
    .

rStandardFunksjoner = NEW cls.StdFunk.StandardFunksjoner( ).
rftpSendFile = NEW cls.RFIDEtikett.ftpSendFile( INPUT cLogg ).

FOR EACH SysPara NO-LOCK WHERE 
    SysPara.SysHId = 5 AND 
    SysPara.SysGr  = 30:
        
    ASSIGN 
        cKatalogLst = cKatalogLst + 
                      (IF cKatalogLst <> '' THEN ',' ELSE '') + 
                      SysPara.Parameter1.    
END.

IF cKatalogLst = '' THEN 
    ASSIGN 
        cKatalogLst = 'filer\,C:\OpenEdge\WRK\filer\'
        .
ASSIGN         
    cEkstent    = '.csv'
    cPrefix     = 'ETI'
/*    cTimeLst    = '23,01'*/
    cTimeLst    = ''
    bTest       = FALSE 
    .

EVIGHETEN:
DO WHILE TRUE:
    DO iLoop = 1 TO NUM-ENTRIES(cKatalogLst):
        cKatalog = ENTRY(iLoop,cKatalogLst).
        
        ETILOOP:
        FOR EACH SysPara NO-LOCK WHERE 
            SysPara.SysHId = 5 AND 
            SysPara.SysGr  = 29:
        
            OS-CREATE-DIR VALUE(cKatalog + '\' + STRING(SysPara.ParaNr)) NO-ERROR. 
        
            /* Tid på døgnet for backup m.m. */
            ASSIGN 
                cTime = ENTRY(1,STRING(TIME,"HH:MM:SS"),':').
            IF cTimeLst <> '' AND CAN-DO(cTimeLst,cTime) OR SEARCH('rfid_stop.txt') <> ? THEN 
                LEAVE EVIGHETEN.
        
            /* Sikrer at temp-tabellen er tom før den fylles på. */
            EMPTY TEMP-TABLE  tmpFiler.
            
            /* Henter liste med filer som skal sendes for butikken. */
            rStandardFunksjoner:LagFillisteForKatalog(INPUT  cKatalog + STRING(SysPara.ParaNr),
                                                      INPUT  cPrefix + '_' + STRING(SysPara.ParaNr), 
                                                      INPUT  cEkstent, 
                                                      OUTPUT TABLE tmpFiler).
            IF bTest THEN 
                TEMP-TABLE tmpFiler:WRITE-JSON('file', 'konv\FilLst' + cPrefix + '_' + STRING(SysPara.ParaNr)+ '_' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME,"HH:MM:SS"),':','') + '.JSon', TRUE).
        
            /* Ip.adr Host */
            cTekst = ENTRY(1,SysPara.Parameter1).
        
            IF NUM-ENTRIES(cTekst,'.') = 4 THEN
            DO:
                FOR EACH tmpFiler:
                    rStandardFunksjoner:SkrivTilLogg(cLogg, 
                        '   Sender fil: ' + tmpfiler.Full-Path-Name
                        ). 
                    
                    IF SEARCH(tmpfiler.Full-Path-Name) <> ? THEN 
                        rftpSendFile:SendFile (tmpfiler.Full-Path-Name,
                            cTekst,
                            OUTPUT bOk,
                            OUTPUT cReturn
                            ).
                    DELETE tmpfiler.
                END.
            END.    
        END. /* ETILOOP */
    END.    
    PAUSE 2 NO-MESSAGE.
END. /* EVIGHETEN */

QUIT.

/* **********************  Internal Procedures  *********************** */

