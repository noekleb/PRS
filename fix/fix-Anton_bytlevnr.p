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

run ByttLevNr(101, 767).
run ByttLevNr(156, 155).
run ByttLevNr(498, 427).
run ByttLevNr(428, 455).
run ByttLevNr(3010, 552).
run ByttLevNr(34652, 622).
run ByttLevNr(21, 678).
run ByttLevNr(1221, 682).
run ByttLevNr(22, 693).
run ByttLevNr(189, 702).
run ByttLevNr(10000, 717).
run ByttLevNr(21254, 732).
run ByttLevNr(71, 738).
run ByttLevNr(222, 754).
run ByttLevNr(383, 759).
run ByttLevNr(826, 760).
run ByttLevNr(78, 764).
run ByttLevNr(98, 766).
run ByttLevNr(79, 779).
run ByttLevNr(9991, 785).
run ByttLevNr(10060, 925).
run ByttLevNr(200000, 926).
run ByttLevNr(1020, 927).
run ByttLevNr(17, 928).
run ByttLevNr(20001, 929).
run ByttLevNr(51, 930).
run ByttLevNr(2020, 931).
run ByttLevNr(2450, 932).
run ByttLevNr(10001, 933).
run ByttLevNr(666, 934).
run ByttLevNr(24, 935).
run ByttLevNr(16, 936).
run ByttLevNr(2024, 937).
run ByttLevNr(1264, 938).
run ByttLevNr(4600, 939).
run ByttLevNr(641, 940).
run ByttLevNr(28, 941).
run ByttLevNr(210, 942).
run ByttLevNr(6584, 943).
run ByttLevNr(999, 944).
run ByttLevNr(1113, 945).
run ByttLevNr(7000, 946).
run ByttLevNr(58, 947).
run ByttLevNr(11101, 948).
run ByttLevNr(2456, 949).
run ByttLevNr(30, 950).
run ByttLevNr(399, 951).
run ByttLevNr(160, 952).
run ByttLevNr(152, 953).
run ByttLevNr(100, 954).
run ByttLevNr(172, 955).
run ByttLevNr(67, 956).
run ByttLevNr(8000, 957).
run ByttLevNr(29, 958).
run ByttLevNr(4564, 959).
run ByttLevNr(5000, 961).
run ByttLevNr(1008, 962).
run ByttLevNr(1065, 963).
run ByttLevNr(3022, 966).

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-byttLevNr) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE byttLevNr Procedure 
PROCEDURE byttLevNr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

def input parameter iOldLevNr as int no-undo.
def input parameter iNewLevNr as int no-undo.

/* Skal ikke konverteres, har lest inn leverandører fra Sport1. 
for each LevBas where LevBas.levnr = iOldLevNr:
    LevBas.levnr = iNewLevNr.
end.
*/

for each EkstVPILev where EkstVPILev.LevNr = iOldLevNr:
    EkstVPILev.LevNr = iNewLevNr.
end.

for each Forsalj where Forsalj.LevNr = iOldLevNr:
    Forsalj.LevNr = iNewLevNr.
end.

for each Individ where Individ.levnr = iOldLevNr:
    Individ.levnr = iNewLevNr.
end.

for each LevLager where LevLager.LevNr = iOldLevNr:
    LevLager.LevNr = iNewLevNr.
end.

for each ArtBas where ArtBas.LevNr = iOldLevNr:
    ArtBas.LevNr = iNewLevNr.
end.

for each ReklamasjonsLogg where ReklamasjonsLogg.LevNr = iOldLevNr:
    ReklamasjonsLogg.LevNr = iNewLevNr.
end.

for each Bilderegister where Bilderegister.LevNr = iOldLevNr:
    Bilderegister.LevNr = iNewLevNr.
end.

for each LevSort where LevSort.LevNr = iOldLevNr:
    LevSort.LevNr = iNewLevNr.
end.

for each LevSAnt where LevSAnt.LevNr = iOldLevNr:
    LevSAnt.LevNr = iNewLevNr.
end.

for each ArtPris where ArtPris.LevNr = iOldLevNr:
    ArtPris.LevNr = iNewLevNr.
end.

for each LevPris where LevPris.LevNr = iOldLevNr:
    LevPris.LevNr = iNewLevNr.
end.

for each PrisKo where PrisKo.LevNr = iOldLevNr:
    PrisKo.LevNr = iNewLevNr.
end.

for each BestHode where BestHode.LevNr = iOldLevNr:
    BestHode.LevNr = iNewLevNr.
end.

for each Ordre where Ordre.LevNr = iOldLevNr:
    Ordre.LevNr = iNewLevNr.
end.

for each TransLogg where TransLogg.LevNr = iOldLevNr:
    TransLogg.LevNr = iNewLevNr.
end.

for each TelleLinje where TelleLinje.LevNr = iOldLevNr:
    TelleLinje.LevNr = iNewLevNr.
end.

for each HPrisKo where HPrisKo.LevNr = iOldLevNr:
    HPrisKo.LevNr = iNewLevNr.
end.

for each KundeTrans where KundeTrans.LevNr = iOldLevNr:
    KundeTrans.LevNr = iNewLevNr.
end.

for each MedTrans where MedTrans.LevNr = iOldLevNr:
    MedTrans.LevNr = iNewLevNr.
end.

for each VarebokTemaHode where VarebokTemaHode.LevNr = iOldLevNr:
    VarebokTemaHode.LevNr = iNewLevNr.
end.

for each AltLevBas where AltLevBas.LevNr = iOldLevNr:
    AltLevBas.LevNr = iNewLevNr.
end.

for each VareBokLinje where VareBokLinje.levnr = iOldLevNr:
    VareBokLinje.levnr = iNewLevNr.
end.

for each VareBehLinje where VareBehLinje.levnr = iOldLevNr:
    VareBehLinje.levnr = iNewLevNr.
end.

for each VareBehBestHode where VareBehBestHode.levnr = iOldLevNr:
    VareBehBestHode.levnr = iNewLevNr.
end.

for each LevKontakt where LevKontakt.LevNr = iOldLevNr:
    LevKontakt.LevNr = iNewLevNr.
end.

for each ArtSort where ArtSort.LevNr = iOldLevNr:
    ArtSort.LevNr = iNewLevNr.
end.

for each BrukerLev where BrukerLev.LevNr = iOldLevNr:
    BrukerLev.LevNr = iNewLevNr.
end.

for each VareBokTemaLinje where VareBokTemaLinje.LevNr = iOldLevNr:
    VareBokTemaLinje.LevNr = iNewLevNr.
end.

for each DefaultLevDato where DefaultLevDato.LevNr = iOldLevNr:
    DefaultLevDato.LevNr = iNewLevNr.
end.

for each Varetrans where Varetrans.LevNr = iOldLevNr:
    Varetrans.LevNr = iNewLevNr.
end.

for each PkSdlLinje where PkSdlLinje.LevNr = iOldLevNr:
    PkSdlLinje.LevNr = iNewLevNr.
end.

for each PkSdlHode where PkSdlHode.levnr = iOldLevNr:
    PkSdlHode.levnr = iNewLevNr.
end.

for each VPIMottak where VPIMottak.LevNr = iOldLevNr:
    VPIMottak.LevNr = iNewLevNr.
end.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

