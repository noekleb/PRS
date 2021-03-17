DEFINE VARIABLE iant AS INTEGER NO-UNDO.
DEFINE VARIABLE iStrTypeId AS INTEGER NO-UNDO.
DEFINE VARIABLE cLogg AS CHARACTER NO-UNDO.

ON WRITE OF ArtBas OVERRIDE 
DO:  
END.

ASSIGN 
    cLogg = 'fix-Strtype_Konv_strtype_artikkel' + REPLACE(STRING(TODAY),'/','') + '_' + REPLACE(STRING(TIME),':','')
    .

CURRENT-WINDOW:WIDTH = 350.
FOR EACH StrType EXCLUSIVE-LOCK WHERE 
    StrType.StrTypeId > 2 AND 
/*    StrType.Intervall = 'XXXL -  XXL' AND*/
/*    StrType.Fordeling = '72,66,67,68,69,70,71' AND*/
    CAN-FIND(FIRST ArtBas WHERE 
             ArtBas.StrTypeId = StrType.StrTypeId)
    BREAK BY StrType.Intervall
          BY StrType.Fordeling 
          BY StrType.StrTypeId:
    iAnt = iant + 1.
    
    /* Plukker første størrelsestypen i bryt gruppen. */
    IF FIRST-OF(StrType.Fordeling) THEN 
    DO:
        iStrtypeId = StrType.StrTypeID.
    END.
    
/*    DISPLAY                   */
/*        StrType.StrTypeId     */
/*        Strtype.Beskrivelse   */
/*        StrType.KortNavn      */
/*        StrType.Intervall     */
/*        StrType.fordeling     */
/*        StrType.AlfaFordeling */
/*        StrType.RegistrertDato*/
/*        StrType.EDato         */
/*    WITH WIDTH 350.           */

/*    UPDATE               */
/*        StrType.Intervall*/
/*        StrType.fordeling*/
/*    WITH WIDTH 350.      */

    /* Leser alle artikler som ligger koblet */
    IF StrType.StrTypeID <> iStrTypeId THEN 
    FOR EACH ArtBas EXCLUSIVE-LOCK WHERE 
        ArtBas.StrTypeID = StrType.StrTypeID:
        
/*        DISPLAY              */
/*            ArtBas.ArtikkelNr*/
/*            ArtBas.Beskr     */
/*            ArtBas.StrTypeId */
/*        WITH WIDTH 350.      */
        
        RUN bibl_logg.p (cLogg,'  Byttet strtype: ' + 
                               STRING(ArtBas.ArtikkelNr) + '/' +
                               ArtBas.Beskr + '/' + 
                               ArtBas.LevKod + '/' + 
                               ArtBas.LevFargKod + '/' +  
                               STRING(ArtBas.StrTypeId) + '/'+ 
                               STRING(iStrTypeId)).
        
        ASSIGN 
            ArtBas.StrTypeID = iStrTypeId
            .            
    END.
    
END.
MESSAGE iAnt
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
