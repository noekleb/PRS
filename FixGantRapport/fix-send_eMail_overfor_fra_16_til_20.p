            
/*------------------------------------------------------------------------
    File        : fix-send_eMail_overfor_fra_16_til_20.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tomn
    Created     : Sun Mar 17 12:51:31 CET 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE iButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE iMButNr AS INTEGER NO-UNDO.
DEFINE VARIABLE dFraDato AS DATE NO-UNDO.
DEFINE VARIABLE iTTId AS INTEGER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
ASSIGN 
    iButNr   = 16
    iMButNr  = 20
    dFraDato = 01/01/2019
    iTTId    = 6
    .

FOR EACH BongHode NO-LOCK WHERE 
    BongHode.ButikkNr = iButNr AND 
    BongHode.Dato    >= dFraDato AND  
    CAN-FIND(FIRST BongLinje WHERE
             BongLinje.b_id = BongHode.b_id AND  
             BongLinje.TTId = iTTId AND 
             BongLinje.MButikkNr = iMButNr):
    
    FIND FIRST BongLinje NO-LOCK WHERE
             BongLinje.b_id = BongHode.b_id AND  
             BongLinje.TTId = iTTId NO-ERROR.

    IF AVAILABLE FakturaLinje THEN RELEASE FakturaLinje.
    IF AVAILABLE FakturaHode  THEN RELEASE FakturaHode.
    FIND FIRST FakturaLinje NO-LOCK WHERE 
        FakturaLinje.B_Id = BongHode.B_Id NO-ERROR.
    IF AVAILABLE FakturaLinje THEN
    DO:
        FIND FakturaHode OF FakturaLinje NO-LOCK NO-ERROR.
        RUN sendFakturaEMail.p ( FakturaHode.Faktura_Id ).
    END.
    
    DISPLAY 
        BongHode.butikkNr
        BongHode.Dato
        BongLinje.MButikkNr
        FakturaHode.FakturaNr
        AVAILABLE FakturaLinje
        AVAILABLE FakturaHode
    .
        
END.        

