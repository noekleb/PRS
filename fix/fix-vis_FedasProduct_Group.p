
/*------------------------------------------------------------------------
    File        : fix-vis_FedasProduct_Group.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : tny
    Created     : Wed Sep 26 15:26:29 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

CURRENT-WINDOW:WIDTH = 350.

FOR EACH FedasProduct_Group NO-LOCK,
    FIRST FedasProduct_Main_Group OF FedasProduct_Group NO-LOCK,
    FIRST FedasProduct_Sub_Group OF FedasProduct_Group NO-LOCK,
    FIRST FedasProduct_Type OF FedasProduct_Group NO-LOCK,
    FIRST FedasActCode OF FedasProduct_Group NO-LOCK:
        
    DISPLAY 
        FedasProduct_Group.ProdGroupId
        FedasProduct_Group.ProdGroupDescription
        FedasProduct_Group.PMGCode
        FedasProduct_Main_Group.Product_Main_Group
        FedasProduct_Group.PSGCode
        FedasProduct_Sub_Group.Product_Sub_Group
        FedasProduct_Group.ProductTypeCode
        FedasProduct_Type.ProductType
        FedasProduct_Group.ActCode
        FedasActCode.Activity
    WITH WIDTH 350.
        
END.