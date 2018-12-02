DEF VAR cLongChar AS LONGCHAR NO-UNDO.
DEF VAR cValueLst AS CHAR NO-UNDO.

ASSIGN
    cLongChar = 
'
<NewCartForTransactionsResult xmlns="http://www.lindbak.no/posservice/itemsale/version1" xmlns:i="http://www.w3.org/2001/XMLSchema-instance" xmlns:s="http://schemas.xmlsoap.org/soap/envelope/">
  <ActionCodes></ActionCodes>
  <Cart>
    <BarCodeData>22101100180000000012000000002310183</BarCodeData>
    <CartId>
      <HashBasedMessageAuthenticationCode>42</HashBasedMessageAuthenticationCode>
      <ID>11001-800-12</ID>
      <Revision>3</Revision>
    </CartId>
    <Currency>NOK</Currency>
    <Customer i:nil="true"></Customer>
    <CustomerOrderNumber i:nil="true"></CustomerOrderNumber>
    <Delivery i:nil="true"></Delivery>
    <ExternalOrderNumber i:nil="true"></ExternalOrderNumber>
    <Items></Items>
    <LoyaltyNumber i:nil="true"></LoyaltyNumber>
    <Note i:nil="true"></Note>
    <OperationPending>false</OperationPending>
    <Payments></Payments>
    <SequenceNumber>12</SequenceNumber>
    <StartTime>2018-10-23T15:21:19.6405509+02:00</StartTime>
    <Totals>
      <CartDiscountTotals></CartDiscountTotals>
      <DeliveredNowCartItemsTotalAmount>0</DeliveredNowCartItemsTotalAmount>
      <SaletotalAmount>0</SaletotalAmount>
      <SaletotalExclusiveVATAmount>0</SaletotalExclusiveVATAmount>
      <VATAmount>0</VATAmount>
    </Totals>
    <TracingId>11001-800-12</TracingId>
    <VATs></VATs>
    <Notes></Notes>
    <Loyalty>
      <IdentifiedByNumber i:nil="true"></IdentifiedByNumber>
      <IdentifiedByType i:nil="true"></IdentifiedByType>
      <LoyaltyNumber i:nil="true"></LoyaltyNumber>
      <AssociationId i:nil="true"></AssociationId>
    </Loyalty>
    <Coupons></Coupons>
  </Cart>
  <ReasonCodes>
    <Code>
      <Description>11801</Description>
      <Id>6</Id>
    </Code>
  </ReasonCodes>
</NewCartForTransactionsResult>'.

RUN cls\SendOvFil\saxd.p (cLongChar, 'ID,StartTime,VATAmount', OUTPUT cValueLst).

MESSAGE cValueLst
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
