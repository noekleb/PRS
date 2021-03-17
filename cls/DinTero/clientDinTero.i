
/*------------------------------------------------------------------------
    File        : clientDinTero.i
    Purpose     : 

    Syntax      :

    Description : Temp tabeller som benyttes mot DinTero APIet.

    Author(s)   : tomn
    Created     : Sat Oct 10 17:15:29 CEST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttToken NO-UNDO
  FIELD access_token AS CHARACTER FORMAT "x(40)"
  FIELD expires_in AS INTEGER FORMAT ">>>>>>>9"
  FIELD token_type AS CHARACTER FORMAT "x(15)" 
  FIELD dtRead AS DATETIME FORMAT "99/99/9999 HH:MM:SS" 
  FIELD dtExpires AS DATETIME FORMAT "99/99/9999 HH:MM:SS" 
  .
  
DEFINE TEMP-TABLE ttoJSon NO-UNDO 
  FIELD grant_type AS CHARACTER
  FIELD audience   AS CHARACTER.

DEFINE TEMP-TABLE ttUrl NO-UNDO 
  FIELD return_url AS CHARACTER 
  FIELD callback_url AS CHARACTER
  FIELD merchant_terms_url AS CHARACTER
  .
    
DEFINE TEMP-TABLE ttRefund  NO-UNDO 
  FIELD amount AS DECIMAL 
  FIELD reason AS CHARACTER   
  .
  
DEFINE TEMP-TABLE ttCreateSessionRespons NO-UNDO 
  FIELD cId AS CHARACTER SERIALIZE-NAME 'id'
  FIELD cUrl AS CHARACTER SERIALIZE-NAME 'url'
  .  
  
DEFINE TEMP-TABLE ttSession NO-UNDO SERIALIZE-NAME 'Session' 
  FIELD account_id AS CHARACTER 
  FIELD Id AS CHARACTER 
  FIELD created_at AS DATETIME FORMAT "99/99/9999 HH:MM:SS"
  FIELD expires_at AS DATETIME FORMAT "99/99/9999 HH:MM:SS"
  FIELD updated_at AS DATETIME FORMAT "99/99/9999 HH:MM:SS"
  FIELD customer_ip AS CHARACTER  
  FIELD user_agent AS CHARACTER  
  FIELD transaction_id AS CHARACTER  
  FIELD cancelled_by AS CHARACTER  
  FIELD cancelled_at AS DATETIME FORMAT "99/99/9999 HH:MM:SS" 
  . 

DEFINE TEMP-TABLE ttEvents NO-UNDO SERIALIZE-NAME 'events'
  FIELD id AS CHARACTER 
  FIELD name AS CHARACTER 
  FIELD created_at AS DATETIME FORMAT "99/99/9999 HH:MM:SS"
  .
  
/*        {                                                                                               */
/*            "id": "da532ad7-bb19-4e95-a169-10ea2e2fa4ec",                                               */
/*            "name": "VISITED",                                                                          */
/*            "details": {                                                                                */
/*                "amount": 6189,                                                                         */
/*                "payex:payment:id": "\/psp\/creditcard\/payments\/5c0f9802-05cf-428d-515c-08d8749a2745",*/
/*                "payment_product_type": "payex.creditcard",                                             */
/*                "payex:payment:payee_info:payee_id": "95e8a159-6abd-4c72-bcf5-6d0c505c701b",            */
/*                "payex:payment:payee_info:payee_name": "DINTERO AS"                                     */
/*            },                                                                                          */
/*            "created_at": "2020-10-22T06:30:22.634Z"                                                    */
/*        },                                                                                              */
  
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
