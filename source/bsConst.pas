(******************************************************)
(*                                                    *)
(*            Besa Software Classes Library           *)
(*                                                    *)
(*  Copyright (c) 2011-2016 BesaSoftware Corporation  *)
(*           http://www.besasoftware.com              *)
(*                                                    *)
(******************************************************)

/// <summary>
/// Constant Definitions 
/// </summary>
unit bsConst;

interface
const

  NSDelim = ':';
  SXML = 'xml';
  SVersion = 'version';
  SEncoding = 'encoding';
  SStandalone = 'standalone';
  SContentType = 'Content-Type';
  SUserAgent= 'User-Agent';                              { Do not localize }
  SConnection='Connection';
  SKeepAlive='Keep-Alive';
  SAgent='Besa Soap';

  SXMLNS = 'xmlns';
  SHttp = 'http:/';
  SXMLNamespaceURI = SHttp+'/www.w3.org/2000/xmlns/';
  SXMLPrefixNamespaceURI = SHttp+'/www.w3.org/XML/1998/namespace';

  SXMLSchemaNameSpacePre = 'xsd';            { do not localize }
  SXMLSchemaInstNameSpace99Pre = 'xsi';      { do not localize }
  SSoapEncodingPre = 'SOAP-ENC';             { do not localize }
  SSoapEncodingAttr = 'encodingStyle';                     { do not localize }
  SSoap11EncodingS5 = 'http://schemas.xmlsoap.org/soap/encoding/';  { do not localize }
  SSoap12EncodingNamespace = 'http://www.w3.org/2003/05/soap-encoding';  { do not localize }
  SOAPEncodingNamespaces: array[Boolean] of string = (SSoap11EncodingS5, SSoap12EncodingNamespace);

  SSoapEncodingArray = 'Array';                            { do not localize }
  SSoapEncodingArrayType = 'arrayType';                    { do not localize }
  SSoap12EncodingArrayType = 'itemType';                   { do not localize }
  SSoapEncodingArrayTypes: array[Boolean] of string = (SSoapEncodingArrayType, SSoap12EncodingArrayType);

  SSoap12EncodingArraySize = 'arraySize';                   { do not localize }

  ContentTypeUTF8 = 'text/xml; charset=utf-8';             { Do not localize }
  ContentTypeNoUTF8 = 'text/xml';                          { Do not localize }

  ContentType12UTF8 = 'application/soap+xml; charset=utf-8';   { Do not localize }
  ContentType12NoUTF8 = 'application/soap+xml';                { Do not localize }

  ContentTypeTemplate = 'Content-Type: %s';                         { Do not localize }
  ContentTypeWithActionFmt = 'Content-Type: %s;action="%s"';   { Do not localize }
  ContentTypeWithActionNoLabelFmt = '%s;action="%s"';          { Do not localize }

  SSoapHTTPTransport = 'http://schemas.xmlsoap.org/soap/http';   { do not localize }
  SSoapBodyUseEncoded = 'encoded';                         { do not localize }
  SSoapBodyUseLiteral = 'literal';                         { do not localize }

  SSoapNameSpacePre = 'SOAP-ENV';            { do not localize }
  SSoapEnvelope = 'Envelope';                              { do not localize }
  SSoapHeader = 'Header';                                  { do not localize }
  SSoapBody = 'Body';                                      { do not localize }
  SSoapResponseSuff = 'Response';                          { do not localize }

  SOperationName='%OperationName%';
  SHTTPSoapAction = 'SOAPAction';

  SSoapServerFaultCode = 'Server';                         { do not localize }
  SSoapServerFaultString = 'Server Error';                 { do not localize }
  SSoapFault = 'Fault';                                    { do not localize }
  SSoapFaultCode = 'faultcode';                            { do not localize }
  SSoapFaultString = 'faultstring';                        { do not localize }
  SSoapFaultActor = 'faultactor';                          { do not localize }
  SSoapFaultDetails =  'detail';                           { do not localize }
  SFaultCodeMustUnderstand = 'MustUnderstand';             { do not localize }

  SSOAP12FaultCode = 'Code';
  SSOAP12FaultSubCode = 'Subcode';
  SSOAP12FaultReason = 'Reason';
  SSOAP12FaultReasonLang = 'lang';
  SSOAP12FaultNode = 'Node';
  SSOAP12FaultRole = 'Role';
  SSOAP12FaultDetail = 'Detail';
  SSOAP12FaultText = 'Text';



  SSoapNamespace = 'http://schemas.xmlsoap.org/soap/envelope/'; { do not localize }
  SSoap12Namespace = 'http://www.w3.org/2003/05/soap-envelope'; { do not localize }
  SOAPEnvelopeNamespaces: array[Boolean] of string = (SSoapNamespace, SSoap12Namespace);

  QualifiedFormNames: array[Boolean] of string = ('sfUnqualified', 'sfQualified');

  SXMLSchemaExt = '.xsd';   { Default }
  SXMLSchemaPrefix = 'xs';  { Default }
  SXMLSchemaLocation = 'schemaLocation';
  SXMLNoNSSchemaLocation = 'noNamespaceSchemaLocation';
  { Working Drafts - 5 November 1999 to 7 April 2000  }
  SXMLSchemaURI_1999 = SHttp+'/www.w3.org/1999/XMLSchema';
  SXMLSchemaInstURI_1999 = SHttp+'/www.w3.org/1999/XMLSchema-instance';
  { Working Draft 22 September 2000 to Candidate Rec. 10 October 2000 }
  SXMLSchemaURI_2000_10 = SHttp+'/www.w3.org/2000/10/XMLSchema';
  SXMLSchemaInstURI_2000_10 = SHttp+'/www.w3.org/2000/10/XMLSchema-instance';
  { Proposed Rec. 16 March 2001 to Final Rec. 02 May 2001  }
  SXMLSchemaURI_2001 = SHttp+'/www.w3.org/2001/XMLSchema';
  SXMLSchemaInstURI = SHttp+'/www.w3.org/2001/XMLSchema-instance';

  SWSA_2005_08='http://www.w3.org/2005/08/addressing';
  SWSA_2004_08='http://schemas.xmlsoap.org/ws/2004/08/addressing';
  SWSAPre = 'wsa';
var

  XMLSchemaNameSpace: string = SXMLSchemaURI_2001;    { Default namespace we publish under }
  XMLSchemaInstNameSpace: string =  SXMLSchemaInstURI;

implementation

end.


