/* fix-test_connect_UniversalDB.p */

USING Progress.Lang.*.
USING System.Data.SqlClient.*.
USING System.Data.*.

/* Kommunikasjonsparametre */
DEFINE VARIABLE cPwd    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserId AS CHARACTER NO-UNDO.
DEFINE VARIABLE cServer AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDbName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDataSource AS CHARACTER NO-UNDO.

/* Oppkobling mot server. */
DEFINE VARIABLE cSQL      AS CHARACTER                    NO-UNDO.
DEFINE VARIABLE ConString AS CHARACTER                    NO-UNDO.
DEFINE VARIABLE Conn      AS System.Data.SqlClient.SqlConnection NO-UNDO.
DEFINE VARIABLE Cmd       AS SqlCommand                          NO-UNDO.
DEFINE VARIABLE CmdRead   AS SqlCommand                          NO-UNDO.
DEFINE VARIABLE Rdr       AS SqlDataReader                       NO-UNDO.
DEFINE VARIABLE SqlCred   AS SqlCredential                       NO-UNDO.
DEFINE VARIABLE SeqString AS System.Security.SecureString        NO-UNDO.

DEFINE VARIABLE cLogg    AS CHARACTER NO-UNDO.
DEFINE VARIABLE bTest    AS LOG       NO-UNDO.
DEFINE VARIABLE cDatoTid AS CHARACTER NO-UNDO.  
DEFINE VARIABLE cTekst   AS CHARACTER NO-UNDO.  
DEFINE VARIABLE iX       AS INTEGER   NO-UNDO.
DEFINE VARIABLE bOk      AS LOG       NO-UNDO.
DEFINE VARIABLE cVg AS CHARACTER NO-UNDO.
DEFINE VARIABLE piLoop AS INTEGER NO-UNDO.
DEFINE VARIABLE piLopNr AS INTEGER NO-UNDO.

/* Endringer her skal ikke utløse ny ELogg post og resending av ordre. */    
ON CREATE OF ArtBas OVERRIDE DO: END.
ON WRITE  OF ArtBas OVERRIDE DO: END.
ON CREATE OF ArtBas OVERRIDE DO: END.

{cls\UNI2\tmpTblvArticle_NO.i}

CURRENT-WINDOW:WIDTH = 350.

ON CREATE OF artbas OVERRIDE DO: END.
ON WRITE OF artbas  OVERRIDE DO: END.

/* Kommunikasjonsparametre */ 
ASSIGN 
    cPwd        = 'Uhdsa67RT'
    cUserId     = 'QlickView'
    cServer     = '192.168.100.30'
    cDbName     = 'UniversalDB'
    cDataSource = 'NORGE0047'
    .

RUN oppkoblingSqlServer.

IF bOk THEN
    RUN getData.

IF bOk THEN
    RUN nedkoblingSqlServer.

PROCEDURE getData:
    DEFINE VARIABLE bResult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iAntRecord AS INTEGER NO-UNDO.
    DEFINE VARIABLE piEAN AS INT64 NO-UNDO.
    DEFINE VARIABLE pcArtNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE piAnt AS INTEGER NO-UNDO.

    ARTLOOP:
    FOR EACH ArtBas EXCLUSIVE-LOCK WHERE 
        ArtBas.LevKod > '' AND
        ArtBas.LevFargKod > '' AND 
        ArtBas.Vg >= 100000 AND 
        ArtBas.Vg <= 999999 AND
        ArtBas.OPris = FALSE AND 
        (ArtBas.Anv-Id <= 1 OR 
        ArtBas.HovedKatNr <= 1)
        :
        
        piant = piAnt + 1.
        ASSIGN
            cSQL = "SELECT
               [nSeason]
              ,[nArtKey]
              ,[cArtno]
              ,[cArtName]
              ,[nMemberCode1]
              ,[cMemberName1]
              ,[nMemberCode3]
              ,[cMemberName3]
              FROM [UniversalDB].[dbo].[vArticles]
                where cArtNo = '" + ArtBas.LevKod + "'"
            .
                    
        CmdRead:CommandText = cSQL.

        Rdr = CmdRead:ExecuteReader().

        EMPTY TEMP-TABLE tmpvArticle_NO.
        
        LOOPEN:
        DO WHILE Rdr:Read() ON ERROR UNDO, LEAVE:
            ASSIGN 
                pcArtNo = STRING(Rdr["cArtno"])                
                .
                
            CREATE tmpvArticle_NO.
            ASSIGN 
                tmpvArticle_NO.nSeason    = Rdr["nSeason"]
                tmpvArticle_NO.nArtKey    = Rdr["nArtKey"]
                tmpvArticle_NO.cArtno     = STRING(Rdr["cArtno"])
                tmpvArticle_NO.cArtName   = STRING(Rdr["cArtName"])
                tmpvArticle_NO.nArtGroup  = Rdr["nMemberCode1"]
                tmpvArticle_NO.cArtGroup  = STRING(Rdr["cMemberName1"])
                tmpvArticle_NO.nMainGroup = Rdr["nMemberCode3"]
                tmpvArticle_NO.cMainGroup = STRING(Rdr["cMemberName3"])
                piant = piAnt + 1
                .
            LEAVE LOOPEN.
        END. /* LOOPEN */ 
        Rdr:Close().

        IF AVAILABLE tmpvArticle_NO THEN
        FIXBLOKK:
        DO:
        /* Varegruppen må legges under hovedgruppen. Den kan forekomme flere ganger. */
        cVg = STRING(tmpvArticle_NO.nMainGroup) + FILL('0',4 - LENGTH(STRING(tmpvArticle_NO.nArtGroup))) + STRING(tmpvArticle_NO.nArtGroup).
        
        RUN oppdaterHovedGr.
        RUN oppdaterVareGr.
        RUN oppdaterAnv-Kod.

        ARTBLOKK:
        DO:
          
          DISPLAY
              ArtBas.ArtikkelNr
              ArtBas.Beskr
              ArtBas.LevKod
              ArtBas.LevFargKod
              ArtBas.RegistrertDato
              ArtBas.EDato
              '|'
              ArtBas.Vg
              ArtBas.Hg
              '|'
              ArtBas.HovedKatNr FORMAT ">>>>>9"
              ArtBas.anv-id
              '|'
              tmpvArticle_NO.nArtGroup
              tmpvArticle_NO.cArtGroup
              tmpvArticle_NO.nMainGroup
              tmpvArticle_NO.cMainGroup
          WITH WIDTH 350.
          
          IF ArtBas.Vg <> INT(cVg) THEN
          DO:
            ASSIGN
              ArtBas.Vg     = INT(cVg)
              ArtBas.LopNr  = ?
              ArtBas.Hg     = tmpvArticle_NO.nMainGroup
              ArtBas.Anv-Id = tmpvArticle_NO.nMainGroup
              NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
            DO:
              DO ix = 1 TO ERROR-STATUS:NUM-MESSAGES:
                  cTekst = STRING(ERROR-STATUS:GET-NUMBER(ix)) + ' '+
                           ERROR-STATUS:GET-MESSAGE(ix).
              END.
              MESSAGE cTekst
              VIEW-AS ALERT-BOX.
            END.
            ELSE
              LOOPEN:
              DO piLoop = 1 TO 10:
                piLopNr = 0.
                RUN SettLopNr.p (artbas.vg,"F",OUTPUT piLopNr).
                ASSIGN
                  ArtBas.LopNr = piLopnr NO-ERROR.
                IF ERROR-STATUS:ERROR THEN
                DO:
                  MESSAGE
                      'Feilet med artikkel/løpenr ' + STRING(ArtBAs.ArtikkelNr) + '/' + String(piLopNr) + ' loop: ' + STRING(piLoop)
                  VIEW-AS ALERT-BOX.
                  NEXT.
                END.
                ELSE DO:
                  LEAVE.
                END.
              END. /* LOOPEN */
          END.

          FIND VarGr NO-LOCK WHERE
            VarGr.Vg = ArtBas.Vg NO-ERROR.
          IF AVAILABLE VarGr AND
            NOT CAN-FIND(HovedKategori WHERE
                          HovedKategori.HovedKatNr = tmpvArticle_NO.nArtGroup) THEN
          DO:
              CREATE HovedKategori.
              ASSIGN
                  HovedKategori.HovedKatNr = tmpvArticle_NO.nArtGroup
                  HovedKategori.HovedKatTekst = VarGr.VgBeskr
                  .
          END.

          IF ArtBas.HovedKatNr <> tmpvArticle_NO.nArtGroup THEN
          ASSIGN
              ArtBas.HovedKatNr = tmpvArticle_NO.nArtGroup
              .

          IF ArtBas.Anv-Id <> tmpvArticle_NO.nMainGroup THEN
          ASSIGN
              ArtBas.Anv-Id = tmpvArticle_NO.nMainGroup
              .              
        END. /* ARTBLOKK */
        END. /* FIXBLOKK */
    END. /* ARTLOOP */
    

END PROCEDURE.
    
    
PROCEDURE nedkoblingSqlServer:
    bok = FALSE.

    /* kobler ned forbindelse til SqlServer databasen. */
    Conn:Close() NO-ERROR.
    IF NOT ERROR-STATUS:ERROR THEN
    DO: 
        MESSAGE 
            '  Koblet ned forbindelse til Sql server : ' + ConString + '.'
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        bOk = TRUE.
    END.    
        
END PROCEDURE.

PROCEDURE oppkoblingSqlServer:

    bOk = FALSE.
    
    /* passord -> sec.string */
    SeqString = NEW System.Security.SecureString().
    DO ix = 1 TO LENGTH(cPwd):
        SeqString:AppendChar(SUBSTR(cPwd,ix,1)).
    END.
    SeqString:MakeReadOnly().

    /* brukernavn, pwd */
    SqlCred = NEW SqlCredential(cUserId,SeqString).

    ConString = "Server=" + cServer + ",1433".
    ConString = ConString + ";Data Source=" + cDataSource +  ";Database=" + cDbName .

    Conn = NEW SqlConnection(ConString,SqlCred).

    Conn:Open() NO-ERROR.
    IF ERROR-STATUS:ERROR THEN 
    DO:
        MESSAGE 
            ERROR-STATUS:GET-MESSAGE(1) 
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.
    ELSE 
    DO: 
/*        MESSAGE                                           */
/*            'Oppkoblet mot Sql server: ' + ConString + '.'*/
/*            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.     */
        bOk = TRUE.
    END.   

    CmdRead = NEW SqlCommand('', Conn).

    RETURN.

END PROCEDURE.

PROCEDURE oppdaterHovedGr:

  FIND HuvGr EXCLUSIVE-LOCK WHERE 
    HuvGr.Hg = tmpvArticle_NO.nMainGroup NO-ERROR.
  IF NOT AVAILABLE HuvGr AND NOT LOCKED HuvGr THEN 
  DO:
    CREATE HuvGr.
    ASSIGN 
      HuvGr.Hg         = tmpvArticle_NO.nMainGroup
      HuvGr.HgBeskr    = tmpvArticle_NO.cMainGroup
      HuvGr.AvdelingNr = IF tmpvArticle_NO.cArtGroup = 'HOME' THEN 2 ELSE 1
      .  
  END.
  IF AVAILABLE HuvGr AND NOT LOCKED HuvGr AND 
    HuvGr.HgBeskr <> tmpvArticle_NO.cMainGroup THEN 
  DO:
    ASSIGN 
      HuvGr.HgBeskr    = tmpvArticle_NO.cMainGroup
      HuvGr.AvdelingNr = IF tmpvArticle_NO.cMainGroup = 'HOME' THEN 2 ELSE 1
      .
  END.  
      
END PROCEDURE.

PROCEDURE oppdaterVareGr:
  
  FIND VarGr WHERE
      VarGr.Vg = INT(cVg) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
  IF NOT AVAILABLE VarGr AND NOT LOCKED VarGr THEN 
    DO:
        CREATE VarGr.
        ASSIGN
            VarGr.Vg         = INT(cVg)
            VarGr.VgBeskr    = tmpvArticle_NO.cArtGroup
            VarGr.Hg         = tmpvArticle_NO.nMainGroup
            VarGr.MomsKod    = 1
            VarGr.Kost_Proc  = 65
            NO-ERROR.
            
        FOR EACH Kategori NO-LOCK WHERE
            Kategori.KatNr <= 4:
            IF NOT CAN-FIND(FIRST VgKat WHERE
                            VgKat.Vg    = VarGr.Vg AND
                            VgKat.VgKat = Kategori.KatNr) THEN
            DO:
                CREATE VgKat.
                ASSIGN
                VgKat.Vg    = VarGr.Vg
                VgKat.VgKat = Kategori.KatNr
                VgKat.KatNr = Kategori.KatNr
                NO-ERROR.
            END.
        END.
    END.
  IF NOT LOCKED VarGr AND VarGr.VgBeskr <>  tmpvArticle_NO.cArtGroup THEN
  DO: 
    ASSIGN 
      VarGr.VgBeskr    = tmpvArticle_NO.cArtGroup
      VarGr.Hg         = tmpvArticle_NO.nMainGroup
      NO-ERROR.
  END.    

END PROCEDURE.

PROCEDURE oppdaterAnv-Kod:
  
  FIND Anv-Kod WHERE
      Anv-Kod.Anv-Id = tmpvArticle_NO.nMainGroup EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
  IF NOT AVAILABLE Anv-Kod AND NOT LOCKED Anv-Kod THEN 
    DO:
        CREATE Anv-Kod.
        ASSIGN
            Anv-Kod.Anv-Id   = tmpvArticle_NO.nMainGroup
            Anv-Kod.AnvBeskr = tmpvArticle_NO.cMainGroup
            NO-ERROR.
    END.
END PROCEDURE.

