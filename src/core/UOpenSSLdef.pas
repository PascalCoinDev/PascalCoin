unit UOpenSSLdef;

{ Copyright (c) 2016 by Albert Molina

  Distributed under the MIT software license, see the accompanying file LICENSE
  or visit http://www.opensource.org/licenses/mit-license.php.

  This unit is a part of the PascalCoin Project, an infinitely scalable
  cryptocurrency. Find us here:
  Web: https://pascalcoin.org
  Source: https://github.com/PascalCoin/PascalCoin

  Acknowledgements
  - Portions of this unit contains code from
    https://github.com/Arvur/OpenSSL-Delphi (unspecified license, public code)

  If you like it, consider a donation using Bitcoin:
  16K3HCZRhFUtM8GdWRcfKeaa6KsuyxZaYk

  THIS LICENSE HEADER MUST NOT BE REMOVED.
}

interface

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

uses {$IFDEF UNIX}BaseUnix {$ELSE} Windows{$ENDIF};

{$REGION 'OBJECT'}
const
 SN_undef                                               = 'UNDEF';
 LN_undef                                               = 'undefined';
 NID_undef                                              = 0;
 SN_Algorithm                                           = 'Algorithm';
 LN_algorithm                                           = 'algorithm';
 NID_algorithm                                          = 38;
 LN_rsadsi                                              = 'rsadsi';
 NID_rsadsi                                             = 1;
 LN_pkcs                                                = 'pkcs';
 NID_pkcs                                               = 2;
 SN_md2                                                 = 'MD2';
 LN_md2                                                 = 'md2';
 NID_md2                                                = 3;
 SN_md5                                                 = 'MD5';
 LN_md5                                                 = 'md5';
 NID_md5                                                = 4;
 SN_rc4                                                 = 'RC4';
 LN_rc4                                                 = 'rc4';
 NID_rc4                                                = 5;
 LN_rsaEncryption                                       = 'rsaEncryption';
 NID_rsaEncryption                                      = 6;
 SN_md2WithRSAEncryption                                = 'RSA-MD2';
 LN_md2WithRSAEncryption                                = 'md2WithRSAEncryption';
 NID_md2WithRSAEncryption                               = 7;
 SN_md5WithRSAEncryption                                = 'RSA-MD5';
 LN_md5WithRSAEncryption                                = 'md5WithRSAEncryption';
 NID_md5WithRSAEncryption                               = 8;
 SN_pbeWithMD2AndDES_CBC                                = 'PBE-MD2-DES';
 LN_pbeWithMD2AndDES_CBC                                = 'pbeWithMD2AndDES-CBC';
 NID_pbeWithMD2AndDES_CBC                               = 9;
 SN_pbeWithMD5AndDES_CBC                                = 'PBE-MD5-DES';
 LN_pbeWithMD5AndDES_CBC                                = 'pbeWithMD5AndDES-CBC';
 NID_pbeWithMD5AndDES_CBC                               = 10;
 LN_X500                                                = 'X500';
 NID_X500                                               = 11;
 LN_X509                                                = 'X509';
 NID_X509                                               = 12;
 SN_commonName                                          = 'CN';
 LN_commonName                                          = 'commonName';
 NID_commonName                                         = 13;
 SN_countryName                                         = 'C';
 LN_countryName                                         = 'countryName';
 NID_countryName                                        = 14;
 SN_localityName                                        = 'L';
 LN_localityName                                        = 'localityName';
 NID_localityName                                       = 15;
 SN_stateOrProvinceName                                 = 'ST';
 LN_stateOrProvinceName                                 = 'stateOrProvinceName';
 NID_stateOrProvinceName                                = 16;
 SN_organizationName                                    = 'O';
 LN_organizationName                                    = 'organizationName';
 NID_organizationName                                   = 17;
 SN_organizationalUnitName                              = 'OU';
 LN_organizationalUnitName                              = 'organizationalUnitName';
 NID_organizationalUnitName                             = 18;
 SN_rsa                                                 = 'RSA';
 LN_rsa                                                 = 'rsa';
 NID_rsa                                                = 19;
 LN_pkcs7                                               = 'pkcs7';
 NID_pkcs7                                              = 20;
 LN_pkcs7_data                                          = 'pkcs7-data';
 NID_pkcs7_data                                         = 21;
 LN_pkcs7_signed                                        = 'pkcs7-signedData';
 NID_pkcs7_signed                                       = 22;
 LN_pkcs7_enveloped                                     = 'pkcs7-envelopedData';
 NID_pkcs7_enveloped                                    = 23;
 LN_pkcs7_signedAndEnveloped                            = 'pkcs7-signedAndEnvelopedData';
 NID_pkcs7_signedAndEnveloped                           = 24;
 LN_pkcs7_digest                                        = 'pkcs7-digestData';
 NID_pkcs7_digest                                       = 25;
 LN_pkcs7_encrypted                                     = 'pkcs7-encryptedData';
 NID_pkcs7_encrypted                                    = 26;
 LN_pkcs3                                               = 'pkcs3';
 NID_pkcs3                                              = 27;
 LN_dhKeyAgreement                                      = 'dhKeyAgreement';
 NID_dhKeyAgreement                                     = 28;
 SN_des_ecb                                             = 'DES-ECB';
 LN_des_ecb                                             = 'des-ecb';
 NID_des_ecb                                            = 29;
 SN_des_cfb64                                           = 'DES-CFB';
 LN_des_cfb64                                           = 'des-cfb';
 NID_des_cfb64                                          = 30;
 SN_des_cbc                                             = 'DES-CBC';
 LN_des_cbc                                             = 'des-cbc';
 NID_des_cbc                                            = 31;
 SN_des_ede                                             = 'DES-EDE';
 LN_des_ede                                             = 'des-ede';
 NID_des_ede                                            = 32;
 SN_des_ede3                                            = 'DES-EDE3';
 LN_des_ede3                                            = 'des-ede3';
 NID_des_ede3                                           = 33;
 SN_idea_cbc                                            = 'IDEA-CBC';
 LN_idea_cbc                                            = 'idea-cbc';
 NID_idea_cbc                                           = 34;
 SN_idea_cfb64                                          = 'IDEA-CFB';
 LN_idea_cfb64                                          = 'idea-cfb';
 NID_idea_cfb64                                         = 35;
 SN_idea_ecb                                            = 'IDEA-ECB';
 LN_idea_ecb                                            = 'idea-ecb';
 NID_idea_ecb                                           = 36;
 SN_rc2_cbc                                             = 'RC2-CBC';
 LN_rc2_cbc                                             = 'rc2-cbc';
 NID_rc2_cbc                                            = 37;
 SN_rc2_ecb                                             = 'RC2-ECB';
 LN_rc2_ecb                                             = 'rc2-ecb';
 NID_rc2_ecb                                            = 38;
 SN_rc2_cfb64                                           = 'RC2-CFB';
 LN_rc2_cfb64                                           = 'rc2-cfb';
 NID_rc2_cfb64                                          = 39;
 SN_rc2_ofb64                                           = 'RC2-OFB';
 LN_rc2_ofb64                                           = 'rc2-ofb';
 NID_rc2_ofb64                                          = 40;
 SN_sha                                                 = 'SHA';
 LN_sha                                                 = 'sha';
 NID_sha                                                = 41;
 SN_shaWithRSAEncryption                                = 'RSA-SHA';
 LN_shaWithRSAEncryption                                = 'shaWithRSAEncryption';
 NID_shaWithRSAEncryption                               = 42;
 SN_des_ede_cbc                                         = 'DES-EDE-CBC';
 LN_des_ede_cbc                                         = 'des-ede-cbc';
 NID_des_ede_cbc                                        = 43;
 SN_des_ede3_cbc                                        = 'DES-EDE3-CBC';
 LN_des_ede3_cbc                                        = 'des-ede3-cbc';
 NID_des_ede3_cbc                                       = 44;
 SN_des_ofb64                                           = 'DES-OFB';
 LN_des_ofb64                                           = 'des-ofb';
 NID_des_ofb64                                          = 45;
 SN_idea_ofb64                                          = 'IDEA-OFB';
 LN_idea_ofb64                                          = 'idea-ofb';
 NID_idea_ofb64                                         = 46;
 LN_pkcs9                                               = 'pkcs9';
 NID_pkcs9                                              = 47;
 SN_pkcs9_emailAddress                                  = 'Email';
 LN_pkcs9_emailAddress                                  = 'emailAddress';
 NID_pkcs9_emailAddress                                 = 48;
 LN_pkcs9_unstructuredName                              = 'unstructuredName';
 NID_pkcs9_unstructuredName                             = 49;
 LN_pkcs9_contentType                                   = 'contentType';
 NID_pkcs9_contentType                                  = 50;
 LN_pkcs9_messageDigest                                 = 'messageDigest';
 NID_pkcs9_messageDigest                                = 51;
 LN_pkcs9_signingTime                                   = 'signingTime';
 NID_pkcs9_signingTime                                  = 52;
 LN_pkcs9_countersignature                              = 'countersignature';
 NID_pkcs9_countersignature                             = 53;
 LN_pkcs9_challengePassword                             = 'challengePassword';
 NID_pkcs9_challengePassword                            = 54;
 LN_pkcs9_unstructuredAddress                           = 'unstructuredAddress';
 NID_pkcs9_unstructuredAddress                          = 55;
 LN_pkcs9_extCertAttributes                             = 'extendedCertificateAttributes';
 NID_pkcs9_extCertAttributes                            = 56;
 SN_netscape                                            = 'Netscape';
 LN_netscape                                            = 'Netscape Communications Corp.';
 NID_netscape                                           = 57;
 SN_netscape_cert_extension                             = 'nsCertExt';
 LN_netscape_cert_extension                             = 'Netscape Certificate Extension';
 NID_netscape_cert_extension                            = 58;
 SN_netscape_data_type                                  = 'nsDataType';
 LN_netscape_data_type                                  = 'Netscape Data Type';
 NID_netscape_data_type                                 = 59;
 SN_des_ede_cfb64                                       = 'DES-EDE-CFB';
 LN_des_ede_cfb64                                       = 'des-ede-cfb';
 NID_des_ede_cfb64                                      = 60;
 SN_des_ede3_cfb64                                      = 'DES-EDE3-CFB';
 LN_des_ede3_cfb64                                      = 'des-ede3-cfb';
 NID_des_ede3_cfb64                                     = 61;
 SN_des_ede_ofb64                                       = 'DES-EDE-OFB';
 LN_des_ede_ofb64                                       = 'des-ede-ofb';
 NID_des_ede_ofb64                                      = 62;
 SN_des_ede3_ofb64                                      = 'DES-EDE3-OFB';
 LN_des_ede3_ofb64                                      = 'des-ede3-ofb';
 NID_des_ede3_ofb64                                     = 63;
 SN_sha1                                                = 'SHA1';
 LN_sha1                                                = 'sha1';
 NID_sha1                                               = 64;
 SN_sha1WithRSAEncryption                               = 'RSA-SHA1';
 LN_sha1WithRSAEncryption                               = 'sha1WithRSAEncryption';
 NID_sha1WithRSAEncryption                              = 65;
 SN_dsaWithSHA                                          = 'DSA-SHA';
 LN_dsaWithSHA                                          = 'dsaWithSHA';
 NID_dsaWithSHA                                         = 66;
 SN_dsa_2                                               = 'DSA-old';
 LN_dsa_2                                               = 'dsaEncryption-old';
 NID_dsa_2                                              = 67;
 SN_pbeWithSHA1AndRC2_CBC                               = 'PBE-SHA1-RC2-64';
 LN_pbeWithSHA1AndRC2_CBC                               = 'pbeWithSHA1AndRC2-CBC';
 NID_pbeWithSHA1AndRC2_CBC                              = 68;
  LN_id_pbkdf2                                          = 'PBKDF2';
 NID_id_pbkdf2                                          = 69;
 SN_dsaWithSHA1_2                                       = 'DSA-SHA1-old';
 LN_dsaWithSHA1_2                                       = 'dsaWithSHA1-old';
 NID_dsaWithSHA1_2                                      = 70;
 SN_netscape_cert_type                                  = 'nsCertType';
 LN_netscape_cert_type                                  = 'Netscape Cert Type';
 NID_netscape_cert_type                                 = 71;
 SN_netscape_base_url                                   = 'nsBaseUrl';
 LN_netscape_base_url                                   = 'Netscape Base Url';
 NID_netscape_base_url                                  = 72;
 SN_netscape_revocation_url                             = 'nsRevocationUrl';
 LN_netscape_revocation_url                             = 'Netscape Revocation Url';
 NID_netscape_revocation_url                            = 73;
 SN_netscape_ca_revocation_url                          = 'nsCaRevocationUrl';
 LN_netscape_ca_revocation_url                          = 'Netscape CA Revocation Url';
 NID_netscape_ca_revocation_url                         = 74;
 SN_netscape_renewal_url                                = 'nsRenewalUrl';
 LN_netscape_renewal_url                                = 'Netscape Renewal Url';
 NID_netscape_renewal_url                               = 75;
 SN_netscape_ca_policy_url                              = 'nsCaPolicyUrl';
 LN_netscape_ca_policy_url                              = 'Netscape CA Policy Url';
 NID_netscape_ca_policy_url                             = 76;
 SN_netscape_ssl_server_name                            = 'nsSslServerName';
 LN_netscape_ssl_server_name                            = 'Netscape SSL Server Name';
 NID_netscape_ssl_server_name                           = 77;
 SN_netscape_comment                                    = 'nsComment';
 LN_netscape_comment                                    = 'Netscape Comment';
 NID_netscape_comment                                   = 78;
 SN_netscape_cert_sequence                              = 'nsCertSequence';
 LN_netscape_cert_sequence                              = 'Netscape Certificate Sequence';
 NID_netscape_cert_sequence                             = 79;
 SN_desx_cbc                                            = 'DESX-CBC';
 LN_desx_cbc                                            = 'desx-cbc';
 NID_desx_cbc                                           = 80;
 SN_id_ce                                               = 'id-ce';
 NID_id_ce                                              = 81;
 SN_subject_key_identifier                              = 'subjectKeyIdentifier';
 LN_subject_key_identifier                              = 'X509v3 Subject Key Identifier';
 NID_subject_key_identifier                             = 82;
 SN_key_usage                                           = 'keyUsage';
 LN_key_usage                                           = 'X509v3 Key Usage';
 NID_key_usage                                          = 83;
 SN_private_key_usage_period                            = 'privateKeyUsagePeriod';
 LN_private_key_usage_period                            = 'X509v3 Private Key Usage Period';
 NID_private_key_usage_period                           = 84;
 SN_subject_alt_name                                    = 'subjectAltName';
 LN_subject_alt_name                                    = 'X509v3 Subject Alternative Name';
 NID_subject_alt_name                                   = 85;
 SN_issuer_alt_name                                     = 'issuerAltName';
 LN_issuer_alt_name                                     = 'X509v3 Issuer Alternative Name';
 NID_issuer_alt_name                                    = 86;
 SN_basic_constraints                                   = 'basicConstraints';
 LN_basic_constraints                                   = 'X509v3 Basic Constraints';
 NID_basic_constraints                                  = 87;
 SN_crl_number                                          = 'crlNumber';
 LN_crl_number                                          = 'X509v3 CRL Number';
 NID_crl_number                                         = 88;
 SN_certificate_policies                                = 'certificatePolicies';
 LN_certificate_policies                                = 'X509v3 Certificate Policies';
 NID_certificate_policies                               = 89;
 SN_authority_key_identifier                            = 'authorityKeyIdentifier';
 LN_authority_key_identifier                            = 'X509v3 Authority Key Identifier';
 NID_authority_key_identifier                           = 90;
 SN_bf_cbc                                              = 'BF-CBC';
 LN_bf_cbc                                              = 'bf-cbc';
 NID_bf_cbc                                             = 91;
 SN_bf_ecb                                              = 'BF-ECB';
 LN_bf_ecb                                              = 'bf-ecb';
 NID_bf_ecb                                             = 92;
 SN_bf_cfb64                                            = 'BF-CFB';
 LN_bf_cfb64                                            = 'bf-cfb';
 NID_bf_cfb64                                           = 93;
 SN_bf_ofb64                                            = 'BF-OFB';
 LN_bf_ofb64                                            = 'bf-ofb';
 NID_bf_ofb64                                           = 94;
 SN_mdc2                                                = 'MDC2';
 LN_mdc2                                                = 'mdc2';
 NID_mdc2                                               = 95;
 SN_mdc2WithRSA                                         = 'RSA-MDC2';
 LN_mdc2WithRSA                                         = 'mdc2withRSA';
 NID_mdc2WithRSA                                        = 96;
 SN_rc4_40                                              = 'RC4-40';
 LN_rc4_40                                              = 'rc4-40';
 NID_rc4_40                                             = 97;
 SN_rc2_40_cbc                                          = 'RC2-40-CBC';
 LN_rc2_40_cbc                                          = 'rc2-40-cbc';
 NID_rc2_40_cbc                                         = 98;
 SN_givenName                                           = 'G';
 LN_givenName                                           = 'givenName';
 NID_givenName                                          = 99;
 SN_surname                                             = 'S';
 LN_surname                                             = 'surname';
 NID_surname                                            = 100;
 SN_initials                                            = 'I';
 LN_initials                                            = 'initials';
 NID_initials                                           = 101;
 SN_uniqueIdentifier                                    = 'UID';
 LN_uniqueIdentifier                                    = 'uniqueIdentifier';
 NID_uniqueIdentifier                                   = 102;
 SN_crl_distribution_points                             = 'crlDistributionPoints';
 LN_crl_distribution_points                             = 'X509v3 CRL Distribution Points';
 NID_crl_distribution_points                            = 103;
 SN_md5WithRSA                                          = 'RSA-NP-MD5';
 LN_md5WithRSA                                          = 'md5WithRSA';
 NID_md5WithRSA                                         = 104;
 SN_serialNumber                                        = 'SN';
 LN_serialNumber                                        = 'serialNumber';
 NID_serialNumber                                       = 105;
 SN_title                                               = 'T';
 LN_title                                               = 'title';
 NID_title                                              = 106;
 SN_description                                         = 'D';
 LN_description                                         = 'description';
 NID_description                                        = 107;
 SN_cast5_cbc                                           = 'CAST5-CBC';
 LN_cast5_cbc                                           = 'cast5-cbc';
 NID_cast5_cbc                                          = 108;
 SN_cast5_ecb                                           = 'CAST5-ECB';
 LN_cast5_ecb                                           = 'cast5-ecb';
 NID_cast5_ecb                                          = 109;
 SN_cast5_cfb64                                         = 'CAST5-CFB';
 LN_cast5_cfb64                                         = 'cast5-cfb';
 NID_cast5_cfb64                                        = 110;
 SN_cast5_ofb64                                         = 'CAST5-OFB';
 LN_cast5_ofb64                                         = 'cast5-ofb';
 NID_cast5_ofb64                                        = 111;
 LN_pbeWithMD5AndCast5_CBC                              = 'pbeWithMD5AndCast5CBC';
 NID_pbeWithMD5AndCast5_CBC                             = 112;
 SN_dsaWithSHA1                                         = 'DSA-SHA1';
 LN_dsaWithSHA1                                         = 'dsaWithSHA1';
 NID_dsaWithSHA1                                        = 113;
 NID_md5_sha1                                           = 114;
 SN_md5_sha1                                            = 'MD5-SHA1';
 LN_md5_sha1                                            = 'md5-sha1';
 SN_sha1WithRSA                                         = 'RSA-SHA1-2';
 LN_sha1WithRSA                                         = 'sha1WithRSA';
 NID_sha1WithRSA                                        = 115;
 SN_dsa                                                 = 'DSA';
 LN_dsa                                                 = 'dsaEncryption';
 NID_dsa                                                = 116;
 SN_ripemd160                                           = 'RIPEMD160';
 LN_ripemd160                                           = 'ripemd160';
 NID_ripemd160                                          = 117;
 SN_ripemd160WithRSA                                    = 'RSA-RIPEMD160';
 LN_ripemd160WithRSA                                    = 'ripemd160WithRSA';
 NID_ripemd160WithRSA                                   = 119;
 SN_rc5_cbc                                             = 'RC5-CBC';
 LN_rc5_cbc                                             = 'rc5-cbc';
 NID_rc5_cbc                                            = 120;
 SN_rc5_ecb                                             = 'RC5-ECB';
 LN_rc5_ecb                                             = 'rc5-ecb';
 NID_rc5_ecb                                            = 121;
 SN_rc5_cfb64                                           = 'RC5-CFB';
 LN_rc5_cfb64                                           = 'rc5-cfb';
 NID_rc5_cfb64                                          = 122;
 SN_rc5_ofb64                                           = 'RC5-OFB';
 LN_rc5_ofb64                                           = 'rc5-ofb';
 NID_rc5_ofb64                                          = 123;
 SN_rle_compression                                     = 'RLE';
 LN_rle_compression                                     = 'run length compression';
 NID_rle_compression                                    = 124;
 SN_zlib_compression                                    = 'ZLIB';
 LN_zlib_compression                                    = 'zlib compression';
 NID_zlib_compression                                   = 125;
 SN_ext_key_usage                                       = 'extendedKeyUsage';
 LN_ext_key_usage                                       = 'X509v3 Extended Key Usage';
 NID_ext_key_usage                                      = 126;
 SN_id_pkix                                             = 'PKIX';
 NID_id_pkix                                            = 127;
 SN_id_kp                                               = 'id-kp';
 NID_id_kp                                              = 128;
 SN_server_auth                                         = 'serverAuth';
 LN_server_auth                                         = 'TLS Web Server Authentication';
 NID_server_auth                                        = 129;
 SN_client_auth                                         = 'clientAuth';
 LN_client_auth                                         = 'TLS Web Client Authentication';
 NID_client_auth                                        = 130;
 SN_code_sign                                           = 'codeSigning';
 LN_code_sign                                           = 'Code Signing';
 NID_code_sign                                          = 131;
 SN_email_protect                                       = 'emailProtection';
 LN_email_protect                                       = 'E-mail Protection';
 NID_email_protect                                      = 132;
 SN_time_stamp                                          = 'timeStamping';
 LN_time_stamp                                          = 'Time Stamping';
 NID_time_stamp                                         = 133;
 SN_ms_code_ind                                         = 'msCodeInd';
 LN_ms_code_ind                                         = 'Microsoft Individual Code Signing';
 NID_ms_code_ind                                        = 134;
 SN_ms_code_com                                         = 'msCodeCom';
 LN_ms_code_com                                         = 'Microsoft Commercial Code Signing';
 NID_ms_code_com                                        = 135;
 SN_ms_ctl_sign                                         = 'msCTLSign';
 LN_ms_ctl_sign                                         = 'Microsoft Trust List Signing';
 NID_ms_ctl_sign                                        = 136;
 SN_ms_sgc                                              = 'msSGC';
 LN_ms_sgc                                              = 'Microsoft Server Gated Crypto';
 NID_ms_sgc                                             = 137;
 SN_ms_efs                                              = 'msEFS';
 LN_ms_efs                                              = 'Microsoft Encrypted File System';
 NID_ms_efs                                             = 138;
 SN_ns_sgc                                              = 'nsSGC';
 LN_ns_sgc                                              = 'Netscape Server Gated Crypto';
 NID_ns_sgc                                             = 139;
 SN_delta_crl                                           = 'deltaCRL';
 LN_delta_crl                                           = 'X509v3 Delta CRL Indicator';
 NID_delta_crl                                          = 140;
 SN_crl_reason                                          = 'CRLReason';
 LN_crl_reason                                          = 'CRL Reason Code';
 NID_crl_reason                                         = 141;
 SN_invalidity_date                                     = 'invalidityDate';
 LN_invalidity_date                                     = 'Invalidity Date';
 NID_invalidity_date                                    = 142;
 SN_sxnet                                               = 'SXNetID';
 LN_sxnet                                               = 'Strong Extranet ID';
 NID_sxnet                                              = 143;
 SN_pbe_WithSHA1And128BitRC4                            = 'PBE-SHA1-RC4-128';
 LN_pbe_WithSHA1And128BitRC4                            = 'pbeWithSHA1And128BitRC4';
 NID_pbe_WithSHA1And128BitRC4                           = 144;
 SN_pbe_WithSHA1And40BitRC4                             = 'PBE-SHA1-RC4-40';
 LN_pbe_WithSHA1And40BitRC4                             = 'pbeWithSHA1And40BitRC4';
 NID_pbe_WithSHA1And40BitRC4                            = 145;
 SN_pbe_WithSHA1And3_Key_TripleDES_CBC                  = 'PBE-SHA1-3DES';
 LN_pbe_WithSHA1And3_Key_TripleDES_CBC                  = 'pbeWithSHA1And3-KeyTripleDES-CBC';
 NID_pbe_WithSHA1And3_Key_TripleDES_CBC                 = 146;
 SN_pbe_WithSHA1And2_Key_TripleDES_CBC                  = 'PBE-SHA1-2DES';
 LN_pbe_WithSHA1And2_Key_TripleDES_CBC                  = 'pbeWithSHA1And2-KeyTripleDES-CBC';
 NID_pbe_WithSHA1And2_Key_TripleDES_CBC                 = 147;
 SN_pbe_WithSHA1And128BitRC2_CBC                        = 'PBE-SHA1-RC2-128';
 LN_pbe_WithSHA1And128BitRC2_CBC                        = 'pbeWithSHA1And128BitRC2-CBC';
 NID_pbe_WithSHA1And128BitRC2_CBC                       = 148;
 SN_pbe_WithSHA1And40BitRC2_CBC                         = 'PBE-SHA1-RC2-40';
 LN_pbe_WithSHA1And40BitRC2_CBC                         = 'pbeWithSHA1And40BitRC2-CBC';
 NID_pbe_WithSHA1And40BitRC2_CBC                        = 149;
 LN_keyBag                                              = 'keyBag';
 NID_keyBag                                             = 150;
 LN_pkcs8ShroudedKeyBag                                 = 'pkcs8ShroudedKeyBag';
 NID_pkcs8ShroudedKeyBag                                = 151;
 LN_certBag                                             = 'certBag';
 NID_certBag                                            = 152;
 LN_crlBag                                              = 'crlBag';
 NID_crlBag                                             = 153;
 LN_secretBag                                           = 'secretBag';
 NID_secretBag                                          = 154;
 LN_safeContentsBag                                     = 'safeContentsBag';
 NID_safeContentsBag                                    = 155;
 LN_friendlyName                                        = 'friendlyName';
 NID_friendlyName                                       = 156;
 LN_localKeyID                                          = 'localKeyID';
    NID_localKeyID                                      = 157;
 LN_x509Certificate                                     = 'x509Certificate';
    NID_x509Certificate                                 = 158;
 LN_sdsiCertificate                                     = 'sdsiCertificate';
    NID_sdsiCertificate                                 = 159;
 LN_x509Crl                                             = 'x509Crl';
    NID_x509Crl                                         = 160;
 LN_pbes2                                               = 'PBES2';
 NID_pbes2                                              = 161;
 LN_pbmac1                                              = 'PBMAC1';
 NID_pbmac1                                             = 162;
  LN_hmacWithSHA1                                       = 'hmacWithSHA1';
 NID_hmacWithSHA1                                       = 163;
 LN_id_qt_cps                                           = 'Policy Qualifier CPS';
 SN_id_qt_cps                                           = 'id-qt-cps';
 NID_id_qt_cps                                          = 164;
 LN_id_qt_unotice                                       = 'Policy Qualifier User Notice';
 SN_id_qt_unotice                                       = 'id-qt-unotice';
 NID_id_qt_unotice                                      = 165;
 SN_rc2_64_cbc                                          = 'RC2-64-CBC';
 LN_rc2_64_cbc                                          = 'rc2-64-cbc';
 NID_rc2_64_cbc                                         = 166;
 SN_SMIMECapabilities                                   = 'SMIME-CAPS';
 LN_SMIMECapabilities                                   = 'S/MIME Capabilities';
 NID_SMIMECapabilities                                  = 167;
 SN_pbeWithMD2AndRC2_CBC                                = 'PBE-MD2-RC2-64';
 LN_pbeWithMD2AndRC2_CBC                                = 'pbeWithMD2AndRC2-CBC';
 NID_pbeWithMD2AndRC2_CBC                               = 168;
 SN_pbeWithMD5AndRC2_CBC                                = 'PBE-MD5-RC2-64';
 LN_pbeWithMD5AndRC2_CBC                                = 'pbeWithMD5AndRC2-CBC';
 NID_pbeWithMD5AndRC2_CBC                               = 169;
 SN_pbeWithSHA1AndDES_CBC                               = 'PBE-SHA1-DES';
 LN_pbeWithSHA1AndDES_CBC                               = 'pbeWithSHA1AndDES-CBC';
 NID_pbeWithSHA1AndDES_CBC                              = 170;
 LN_ms_ext_req                                          = 'Microsoft Extension Request';
 SN_ms_ext_req                                          = 'msExtReq';
 NID_ms_ext_req                                         = 171;
 LN_ext_req                                             = 'Extension Request';
 SN_ext_req                                             = 'extReq';
 NID_ext_req                                            = 172;
 SN_name                                                = 'name';
 LN_name                                                = 'name';
 NID_name                                               = 173;
 SN_dnQualifier                                         = 'dnQualifier';
 LN_dnQualifier                                         = 'dnQualifier';
 NID_dnQualifier                                        = 174;
 SN_id_pe                                               = 'id-pe';
 NID_id_pe                                              = 175;
 SN_id_ad                                               = 'id-ad';
 NID_id_ad                                              = 176;
 SN_info_access                                         = 'authorityInfoAccess';
 LN_info_access                                         = 'Authority Information Access';
 NID_info_access                                        = 177;
 SN_ad_OCSP                                             = 'OCSP';
 LN_ad_OCSP                                             = 'OCSP';
 NID_ad_OCSP                                            = 178;
 SN_ad_ca_issuers                                       = 'caIssuers';
 LN_ad_ca_issuers                                       = 'CA Issuers';
 NID_ad_ca_issuers                                      = 179;
 SN_OCSP_sign                                           = 'OCSPSigning';
 LN_OCSP_sign                                           = 'OCSP Signing';
 NID_OCSP_sign                                          = 180;
 SN_itu_t                                               = 'ITU-T';
 LN_itu_t                                               = 'itu-t';
 NID_itu_t                                              = 645;
 NID_ccitt                                              = 404;
 SN_iso                                                 = 'ISO';
 LN_iso                                                 = 'iso';
 NID_iso                                                = 181;
 SN_joint_iso_itu_t                                     = 'JOINT-ISO-ITU-T';
 LN_joint_iso_itu_t                                     = 'joint-iso-itu-t';
 NID_joint_iso_itu_t                                    = 646;
 NID_joint_iso_ccitt                                    = 393;
 SN_member_body                                         = 'member-body';
 LN_member_body                                         = 'ISO Member Body';
 NID_member_body                                        = 182;
 SN_identified_organization                             = 'identified-organization';
 NID_identified_organization                            = 676;
 SN_hmac_md5                                            = 'HMAC-MD5';
 LN_hmac_md5                                            = 'hmac-md5';
 NID_hmac_md5                                           = 780;
 SN_hmac_sha1                                           = 'HMAC-SHA1';
 LN_hmac_sha1                                           = 'hmac-sha1';
 NID_hmac_sha1                                          = 781;
 SN_certicom_arc                                        = 'certicom-arc';
 NID_certicom_arc                                       = 677;
 SN_international_organizations                         = 'international-organizations';
 LN_international_organizations                         = 'International Organizations';
 NID_international_organizations                        = 647;
 SN_wap                                                 = 'wap';
 NID_wap                                                = 678;
 SN_wap_wsg                                             = 'wap-wsg';
 NID_wap_wsg                                            = 679;
  SN_selected_attribute_types                           = 'selected-attribute-types';
 LN_selected_attribute_types                            = 'Selected Attribute Types';
 NID_selected_attribute_types                           = 394;
 SN_clearance                                           = 'clearance';
 NID_clearance                                          = 395;
 SN_ISO_US                                              = 'ISO-US';
 LN_ISO_US                                              = 'ISO US Member Body';
 NID_ISO_US                                             = 183;
 SN_X9_57                                               = 'X9-57';
 LN_X9_57                                               = 'X9.57';
 NID_X9_57                                              = 184;
 SN_X9cm                                                = 'X9cm';
 LN_X9cm                                                = 'X9.57 CM ?';
 NID_X9cm                                               = 185;
 SN_ansi_X9_62                                          = 'ansi-X9-62';
 LN_ansi_X9_62                                          = 'ANSI X9.62';
 NID_ansi_X9_62                                         = 405;
 SN_X9_62_prime_field                                   = 'prime-field';
 NID_X9_62_prime_field                                  = 406;
 SN_X9_62_characteristic_two_field                      = 'characteristic-two-field';
 NID_X9_62_characteristic_two_field                     = 407;
 SN_X9_62_id_characteristic_two_basis                   = 'id-characteristic-two-basis';
 NID_X9_62_id_characteristic_two_basis                  = 680;
 SN_X9_62_onBasis                                       = 'onBasis';
 NID_X9_62_onBasis                                      = 681;
 SN_X9_62_tpBasis                                       = 'tpBasis';
 NID_X9_62_tpBasis                                      = 682;
 SN_X9_62_ppBasis                                       = 'ppBasis';
 NID_X9_62_ppBasis                                      = 683;
 SN_X9_62_id_ecPublicKey                                = 'id-ecPublicKey';
 NID_X9_62_id_ecPublicKey                               = 408;
 SN_X9_62_c2pnb163v1                                    = 'c2pnb163v1';
 NID_X9_62_c2pnb163v1                                   = 684;
 SN_X9_62_c2pnb163v2                                    = 'c2pnb163v2';
 NID_X9_62_c2pnb163v2                                   = 685;
 SN_X9_62_c2pnb163v3                                    = 'c2pnb163v3';
 NID_X9_62_c2pnb163v3                                   = 686;
 SN_X9_62_c2pnb176v1                                    = 'c2pnb176v1';
 NID_X9_62_c2pnb176v1                                   = 687;
 SN_X9_62_c2tnb191v1                                    = 'c2tnb191v1';
 NID_X9_62_c2tnb191v1                                   = 688;
 SN_X9_62_c2tnb191v2                                    = 'c2tnb191v2';
 NID_X9_62_c2tnb191v2                                   = 689;
 SN_X9_62_c2tnb191v3                                    = 'c2tnb191v3';
 NID_X9_62_c2tnb191v3                                   = 690;
 SN_X9_62_c2onb191v4                                    = 'c2onb191v4';
 NID_X9_62_c2onb191v4                                   = 691;
 SN_X9_62_c2onb191v5                                    = 'c2onb191v5';
 NID_X9_62_c2onb191v5                                   = 692;
 SN_X9_62_c2pnb208w1                                    = 'c2pnb208w1';
 NID_X9_62_c2pnb208w1                                   = 693;
 SN_X9_62_c2tnb239v1                                    = 'c2tnb239v1';
 NID_X9_62_c2tnb239v1                                   = 694;
 SN_X9_62_c2tnb239v2                                    = 'c2tnb239v2';
 NID_X9_62_c2tnb239v2                                   = 695;
 SN_X9_62_c2tnb239v3                                    = 'c2tnb239v3';
 NID_X9_62_c2tnb239v3                                   = 696;
 SN_X9_62_c2onb239v4                                    = 'c2onb239v4';
 NID_X9_62_c2onb239v4                                   = 697;
 SN_X9_62_c2onb239v5                                    = 'c2onb239v5';
 NID_X9_62_c2onb239v5                                   = 698;
 SN_X9_62_c2pnb272w1                                    = 'c2pnb272w1';
 NID_X9_62_c2pnb272w1                                   = 699;
 SN_X9_62_c2pnb304w1                                    = 'c2pnb304w1';
 NID_X9_62_c2pnb304w1                                   = 700;
 SN_X9_62_c2tnb359v1                                    = 'c2tnb359v1';
 NID_X9_62_c2tnb359v1                                   = 701;
 SN_X9_62_c2pnb368w1                                    = 'c2pnb368w1';
 NID_X9_62_c2pnb368w1                                   = 702;
 SN_X9_62_c2tnb431r1                                    = 'c2tnb431r1';
 NID_X9_62_c2tnb431r1                                   = 703;
 SN_X9_62_prime192v1                                    = 'prime192v1';
 NID_X9_62_prime192v1                                   = 409;
 SN_X9_62_prime192v2                                    = 'prime192v2';
 NID_X9_62_prime192v2                                   = 410;
 SN_X9_62_prime192v3                                    = 'prime192v3';
 NID_X9_62_prime192v3                                   = 411;
 SN_X9_62_prime239v1                                    = 'prime239v1';
 NID_X9_62_prime239v1                                   = 412;
 SN_X9_62_prime239v2                                    = 'prime239v2';
 NID_X9_62_prime239v2                                   = 413;
 SN_X9_62_prime239v3                                    = 'prime239v3';
 NID_X9_62_prime239v3                                   = 414;
 SN_X9_62_prime256v1                                    = 'prime256v1';
 NID_X9_62_prime256v1                                   = 415;
 SN_ecdsa_with_SHA1                                     = 'ecdsa-with-SHA1';
 NID_ecdsa_with_SHA1                                    = 416;
 SN_ecdsa_with_Recommended                              = 'ecdsa-with-Recommended';
 NID_ecdsa_with_Recommended                             = 791;
 SN_ecdsa_with_Specified                                = 'ecdsa-with-Specified';
 NID_ecdsa_with_Specified                               = 792;
 SN_ecdsa_with_SHA224                                   = 'ecdsa-with-SHA224';
 NID_ecdsa_with_SHA224                                  = 793;
 SN_ecdsa_with_SHA256                                   = 'ecdsa-with-SHA256';
 NID_ecdsa_with_SHA256                                  = 794;
 SN_ecdsa_with_SHA384                                   = 'ecdsa-with-SHA384';
 NID_ecdsa_with_SHA384                                  = 795;
 SN_ecdsa_with_SHA512                                   = 'ecdsa-with-SHA512';
 NID_ecdsa_with_SHA512                                  = 796;
 SN_secp112r1                                           = 'secp112r1';
 NID_secp112r1                                          = 704;
 SN_secp112r2                                           = 'secp112r2';
 NID_secp112r2                                          = 705;
 SN_secp128r1                                           = 'secp128r1';
 NID_secp128r1                                          = 706;
 SN_secp128r2                                           = 'secp128r2';
 NID_secp128r2                                          = 707;
 SN_secp160k1                                           = 'secp160k1';
 NID_secp160k1                                          = 708;
 SN_secp160r1                                           = 'secp160r1';
 NID_secp160r1                                          = 709;
 SN_secp160r2                                           = 'secp160r2';
 NID_secp160r2                                          = 710;
 SN_secp192k1                                           = 'secp192k1';
 NID_secp192k1                                          = 711;
 SN_secp224k1                                           = 'secp224k1';
 NID_secp224k1                                          = 712;
 SN_secp224r1                                           = 'secp224r1';
 NID_secp224r1                                          = 713;
 SN_secp256k1                                           = 'secp256k1';
 NID_secp256k1                                          = 714;
 SN_secp384r1                                           = 'secp384r1';
 NID_secp384r1                                          = 715;
 SN_secp521r1                                           = 'secp521r1';
 NID_secp521r1                                          = 716;
 SN_sect113r1                                           = 'sect113r1';
 NID_sect113r1                                          = 717;
 SN_sect113r2                                           = 'sect113r2';
 NID_sect113r2                                          = 718;
 SN_sect131r1                                           = 'sect131r1';
 NID_sect131r1                                          = 719;
 SN_sect131r2                                           = 'sect131r2';
 NID_sect131r2                                          = 720;
 SN_sect163k1                                           = 'sect163k1';
 NID_sect163k1                                          = 721;
 SN_sect163r1                                           = 'sect163r1';
 NID_sect163r1                                          = 722;
 SN_sect163r2                                           = 'sect163r2';
 NID_sect163r2                                          = 723;
 SN_sect193r1                                           = 'sect193r1';
 NID_sect193r1                                          = 724;
 SN_sect193r2                                           = 'sect193r2';
 NID_sect193r2                                          = 725;
 SN_sect233k1                                           = 'sect233k1';
 NID_sect233k1                                          = 726;
 SN_sect233r1                                           = 'sect233r1';
 NID_sect233r1                                          = 727;
 SN_sect239k1                                           = 'sect239k1';
 NID_sect239k1                                          = 728;
 SN_sect283k1                                           = 'sect283k1';
 NID_sect283k1                                          = 729;
 SN_sect283r1                                           = 'sect283r1';
 NID_sect283r1                                          = 730;
 SN_sect409k1                                           = 'sect409k1';
 NID_sect409k1                                          = 731;
 SN_sect409r1                                           = 'sect409r1';
 NID_sect409r1                                          = 732;
 SN_sect571k1                                           = 'sect571k1';
 NID_sect571k1                                          = 733;
 SN_sect571r1                                           = 'sect571r1';
 NID_sect571r1                                          = 734;
 SN_wap_wsg_idm_ecid_wtls1                              = 'wap-wsg-idm-ecid-wtls1';
 NID_wap_wsg_idm_ecid_wtls1                             = 735;
 SN_wap_wsg_idm_ecid_wtls3                              = 'wap-wsg-idm-ecid-wtls3';
 NID_wap_wsg_idm_ecid_wtls3                             = 736;
 SN_wap_wsg_idm_ecid_wtls4                              = 'wap-wsg-idm-ecid-wtls4';
 NID_wap_wsg_idm_ecid_wtls4                             = 737;
 SN_wap_wsg_idm_ecid_wtls5                              = 'wap-wsg-idm-ecid-wtls5';
 NID_wap_wsg_idm_ecid_wtls5                             = 738;
 SN_wap_wsg_idm_ecid_wtls6                              = 'wap-wsg-idm-ecid-wtls6';
 NID_wap_wsg_idm_ecid_wtls6                             = 739;
 SN_wap_wsg_idm_ecid_wtls7                              = 'wap-wsg-idm-ecid-wtls7';
 NID_wap_wsg_idm_ecid_wtls7                             = 740;
 SN_wap_wsg_idm_ecid_wtls8                              = 'wap-wsg-idm-ecid-wtls8';
 NID_wap_wsg_idm_ecid_wtls8                             = 741;
 SN_wap_wsg_idm_ecid_wtls9                              = 'wap-wsg-idm-ecid-wtls9';
 NID_wap_wsg_idm_ecid_wtls9                             = 742;
 SN_wap_wsg_idm_ecid_wtls10                             = 'wap-wsg-idm-ecid-wtls10';
 NID_wap_wsg_idm_ecid_wtls10                            = 743;
 SN_wap_wsg_idm_ecid_wtls11                             = 'wap-wsg-idm-ecid-wtls11';
 NID_wap_wsg_idm_ecid_wtls11                            = 744;
 SN_wap_wsg_idm_ecid_wtls12                             = 'wap-wsg-idm-ecid-wtls12';
 NID_wap_wsg_idm_ecid_wtls12                            = 745;
 SN_id_PasswordBasedMAC                                 = 'id-PasswordBasedMAC';
 LN_id_PasswordBasedMAC                                 = 'password based MAC';
 NID_id_PasswordBasedMAC                                = 782;
 SN_id_DHBasedMac                                       = 'id-DHBasedMac';
 LN_id_DHBasedMac                                       = 'Diffie-Hellman based MAC';
 NID_id_DHBasedMac                                      = 783;
 SN_rsadsi                                              = 'rsadsi';
 SN_pkcs                                                = 'pkcs';
 SN_pkcs1                                               = 'pkcs1';
 NID_pkcs1                                              = 186;
 SN_md4WithRSAEncryption                                = 'RSA-MD4';
 LN_md4WithRSAEncryption                                = 'md4WithRSAEncryption';
 NID_md4WithRSAEncryption                               = 396;
 SN_rsaesOaep                                           = 'RSAES-OAEP';
 LN_rsaesOaep                                           = 'rsaesOaep';
 NID_rsaesOaep                                          = 919;
 SN_mgf1                                                = 'MGF1';
 LN_mgf1                                                = 'mgf1';
 NID_mgf1                                               = 911;
 SN_rsassaPss                                           = 'RSASSA-PSS';
 LN_rsassaPss                                           = 'rsassaPss';
 NID_rsassaPss                                          = 912;
 SN_sha256WithRSAEncryption                             = 'RSA-SHA256';
 LN_sha256WithRSAEncryption                             = 'sha256WithRSAEncryption';
 NID_sha256WithRSAEncryption                            = 668;
 SN_sha384WithRSAEncryption                             = 'RSA-SHA384';
 LN_sha384WithRSAEncryption                             = 'sha384WithRSAEncryption';
 NID_sha384WithRSAEncryption                            = 669;
 SN_sha512WithRSAEncryption                             = 'RSA-SHA512';
 LN_sha512WithRSAEncryption                             = 'sha512WithRSAEncryption';
 NID_sha512WithRSAEncryption                            = 670;
 SN_sha224WithRSAEncryption                             = 'RSA-SHA224';
 LN_sha224WithRSAEncryption                             = 'sha224WithRSAEncryption';
 NID_sha224WithRSAEncryption                            = 671;
 SN_pkcs3                                               = 'pkcs3';
 SN_pkcs5                                               = 'pkcs5';
 NID_pkcs5                                              = 187;
 SN_SMIME                                               = 'SMIME';
 LN_SMIME                                               = 'S/MIME';
 NID_SMIME                                              = 188;
 SN_id_smime_mod                                        = 'id-smime-mod';
 NID_id_smime_mod                                       = 189;
 SN_id_smime_ct                                         = 'id-smime-ct';
 NID_id_smime_ct                                        = 190;
 SN_id_smime_aa                                         = 'id-smime-aa';
 NID_id_smime_aa                                        = 191;
 SN_id_smime_alg                                        = 'id-smime-alg';
 NID_id_smime_alg                                       = 192;
 SN_id_smime_cd                                         = 'id-smime-cd';
 NID_id_smime_cd                                        = 193;
 SN_id_smime_spq                                        = 'id-smime-spq';
 NID_id_smime_spq                                       = 194;
 SN_id_smime_cti                                        = 'id-smime-cti';
 NID_id_smime_cti                                       = 195;
 SN_id_smime_mod_cms                                    = 'id-smime-mod-cms';
 NID_id_smime_mod_cms                                   = 196;
 SN_id_smime_mod_ess                                    = 'id-smime-mod-ess';
 NID_id_smime_mod_ess                                   = 197;
 SN_id_smime_mod_oid                                    = 'id-smime-mod-oid';
 NID_id_smime_mod_oid                                   = 198;
 SN_id_smime_mod_msg_v3                                 = 'id-smime-mod-msg-v3';
 NID_id_smime_mod_msg_v3                                = 199;
 SN_id_smime_mod_ets_eSignature_88                      = 'id-smime-mod-ets-eSignature-88';
 NID_id_smime_mod_ets_eSignature_88                     = 200;
 SN_id_smime_mod_ets_eSignature_97                      = 'id-smime-mod-ets-eSignature-97';
 NID_id_smime_mod_ets_eSignature_97                     = 201;
 SN_id_smime_mod_ets_eSigPolicy_88                      = 'id-smime-mod-ets-eSigPolicy-88';
 NID_id_smime_mod_ets_eSigPolicy_88                     = 202;
 SN_id_smime_mod_ets_eSigPolicy_97                      = 'id-smime-mod-ets-eSigPolicy-97';
 NID_id_smime_mod_ets_eSigPolicy_97                     = 203;
 SN_id_smime_ct_receipt                                 = 'id-smime-ct-receipt';
 NID_id_smime_ct_receipt                                = 204;
 SN_id_smime_ct_authData                                = 'id-smime-ct-authData';
 NID_id_smime_ct_authData                               = 205;
 SN_id_smime_ct_publishCert                             = 'id-smime-ct-publishCert';
 NID_id_smime_ct_publishCert                            = 206;
 SN_id_smime_ct_TSTInfo                                 = 'id-smime-ct-TSTInfo';
 NID_id_smime_ct_TSTInfo                                = 207;
 SN_id_smime_ct_TDTInfo                                 = 'id-smime-ct-TDTInfo';
 NID_id_smime_ct_TDTInfo                                = 208;
 SN_id_smime_ct_contentInfo                             = 'id-smime-ct-contentInfo';
 NID_id_smime_ct_contentInfo                            = 209;
 SN_id_smime_ct_DVCSRequestData                         = 'id-smime-ct-DVCSRequestData';
 NID_id_smime_ct_DVCSRequestData                        = 210;
 SN_id_smime_ct_DVCSResponseData                        = 'id-smime-ct-DVCSResponseData';
 NID_id_smime_ct_DVCSResponseData                       = 211;
 SN_id_smime_ct_compressedData                          = 'id-smime-ct-compressedData';
 NID_id_smime_ct_compressedData                         = 786;
 SN_id_ct_asciiTextWithCRLF                             = 'id-ct-asciiTextWithCRLF';
 NID_id_ct_asciiTextWithCRLF                            = 787;
 SN_id_smime_aa_receiptRequest                          = 'id-smime-aa-receiptRequest';
 NID_id_smime_aa_receiptRequest                         = 212;
 SN_id_smime_aa_securityLabel                           = 'id-smime-aa-securityLabel';
 NID_id_smime_aa_securityLabel                          = 213;
 SN_id_smime_aa_mlExpandHistory                         = 'id-smime-aa-mlExpandHistory';
 NID_id_smime_aa_mlExpandHistory                        = 214;
 SN_id_smime_aa_contentHint                             = 'id-smime-aa-contentHint';
 NID_id_smime_aa_contentHint                            = 215;
 SN_id_smime_aa_msgSigDigest                            = 'id-smime-aa-msgSigDigest';
 NID_id_smime_aa_msgSigDigest                           = 216;
 SN_id_smime_aa_encapContentType                        = 'id-smime-aa-encapContentType';
 NID_id_smime_aa_encapContentType                       = 217;
 SN_id_smime_aa_contentIdentifier                       = 'id-smime-aa-contentIdentifier';
 NID_id_smime_aa_contentIdentifier                      = 218;
 SN_id_smime_aa_macValue                                = 'id-smime-aa-macValue';
 NID_id_smime_aa_macValue                               = 219;
 SN_id_smime_aa_equivalentLabels                        = 'id-smime-aa-equivalentLabels';
 NID_id_smime_aa_equivalentLabels                       = 220;
 SN_id_smime_aa_contentReference                        = 'id-smime-aa-contentReference';
 NID_id_smime_aa_contentReference                       = 221;
 SN_id_smime_aa_encrypKeyPref                           = 'id-smime-aa-encrypKeyPref';
 NID_id_smime_aa_encrypKeyPref                          = 222;
 SN_id_smime_aa_signingCertificate                      = 'id-smime-aa-signingCertificate';
 NID_id_smime_aa_signingCertificate                     = 223;
 SN_id_smime_aa_smimeEncryptCerts                       = 'id-smime-aa-smimeEncryptCerts';
 NID_id_smime_aa_smimeEncryptCerts                      = 224;
 SN_id_smime_aa_timeStampToken                          = 'id-smime-aa-timeStampToken';
 NID_id_smime_aa_timeStampToken                         = 225;
 SN_id_smime_aa_ets_sigPolicyId                         = 'id-smime-aa-ets-sigPolicyId';
 NID_id_smime_aa_ets_sigPolicyId                        = 226;
 SN_id_smime_aa_ets_commitmentType                      = 'id-smime-aa-ets-commitmentType';
 NID_id_smime_aa_ets_commitmentType                     = 227;
 SN_id_smime_aa_ets_signerLocation                      = 'id-smime-aa-ets-signerLocation';
 NID_id_smime_aa_ets_signerLocation                     = 228;
 SN_id_smime_aa_ets_signerAttr                          = 'id-smime-aa-ets-signerAttr';
 NID_id_smime_aa_ets_signerAttr                         = 229;
 SN_id_smime_aa_ets_otherSigCert                        = 'id-smime-aa-ets-otherSigCert';
 NID_id_smime_aa_ets_otherSigCert                       = 230;
 SN_id_smime_aa_ets_contentTimestamp                    = 'id-smime-aa-ets-contentTimestamp';
 NID_id_smime_aa_ets_contentTimestamp                   = 231;
 SN_id_smime_aa_ets_CertificateRefs                     = 'id-smime-aa-ets-CertificateRefs';
 NID_id_smime_aa_ets_CertificateRefs                    = 232;
 SN_id_smime_aa_ets_RevocationRefs                      = 'id-smime-aa-ets-RevocationRefs';
 NID_id_smime_aa_ets_RevocationRefs                     = 233;
 SN_id_smime_aa_ets_certValues                          = 'id-smime-aa-ets-certValues';
 NID_id_smime_aa_ets_certValues                         = 234;
 SN_id_smime_aa_ets_revocationValues                    = 'id-smime-aa-ets-revocationValues';
 NID_id_smime_aa_ets_revocationValues                   = 235;
 SN_id_smime_aa_ets_escTimeStamp                        = 'id-smime-aa-ets-escTimeStamp';
 NID_id_smime_aa_ets_escTimeStamp                       = 236;
 SN_id_smime_aa_ets_certCRLTimestamp                    = 'id-smime-aa-ets-certCRLTimestamp';
 NID_id_smime_aa_ets_certCRLTimestamp                   = 237;
 SN_id_smime_aa_ets_archiveTimeStamp                    = 'id-smime-aa-ets-archiveTimeStamp';
 NID_id_smime_aa_ets_archiveTimeStamp                   = 238;
 SN_id_smime_aa_signatureType                           = 'id-smime-aa-signatureType';
 NID_id_smime_aa_signatureType                          = 239;
 SN_id_smime_aa_dvcs_dvc                                = 'id-smime-aa-dvcs-dvc';
 NID_id_smime_aa_dvcs_dvc                               = 240;
 SN_id_smime_alg_ESDHwith3DES                           = 'id-smime-alg-ESDHwith3DES';
 NID_id_smime_alg_ESDHwith3DES                          = 241;
 SN_id_smime_alg_ESDHwithRC2                            = 'id-smime-alg-ESDHwithRC2';
 NID_id_smime_alg_ESDHwithRC2                           = 242;
 SN_id_smime_alg_3DESwrap                               = 'id-smime-alg-3DESwrap';
 NID_id_smime_alg_3DESwrap                              = 243;
 SN_id_smime_alg_RC2wrap                                = 'id-smime-alg-RC2wrap';
 NID_id_smime_alg_RC2wrap                               = 244;
 SN_id_smime_alg_ESDH                                   = 'id-smime-alg-ESDH';
 NID_id_smime_alg_ESDH                                  = 245;
 SN_id_smime_alg_CMS3DESwrap                            = 'id-smime-alg-CMS3DESwrap';
 NID_id_smime_alg_CMS3DESwrap                           = 246;
 SN_id_smime_alg_CMSRC2wrap                             = 'id-smime-alg-CMSRC2wrap';
 NID_id_smime_alg_CMSRC2wrap                            = 247;
 SN_id_alg_PWRI_KEK                                     = 'id-alg-PWRI-KEK';
 NID_id_alg_PWRI_KEK                                    = 893;
 SN_id_smime_cd_ldap                                    = 'id-smime-cd-ldap';
 NID_id_smime_cd_ldap                                   = 248;
 SN_id_smime_spq_ets_sqt_uri                            = 'id-smime-spq-ets-sqt-uri';
 NID_id_smime_spq_ets_sqt_uri                           = 249;
 SN_id_smime_spq_ets_sqt_unotice                        = 'id-smime-spq-ets-sqt-unotice';
 NID_id_smime_spq_ets_sqt_unotice                       = 250;
 SN_id_smime_cti_ets_proofOfOrigin                      = 'id-smime-cti-ets-proofOfOrigin';
 NID_id_smime_cti_ets_proofOfOrigin                     = 251;
 SN_id_smime_cti_ets_proofOfReceipt                     = 'id-smime-cti-ets-proofOfReceipt';
 NID_id_smime_cti_ets_proofOfReceipt                    = 252;
 SN_id_smime_cti_ets_proofOfDelivery                    = 'id-smime-cti-ets-proofOfDelivery';
 NID_id_smime_cti_ets_proofOfDelivery                   = 253;
 SN_id_smime_cti_ets_proofOfSender                      = 'id-smime-cti-ets-proofOfSender';
 NID_id_smime_cti_ets_proofOfSender                     = 254;
 SN_id_smime_cti_ets_proofOfApproval                    = 'id-smime-cti-ets-proofOfApproval';
 NID_id_smime_cti_ets_proofOfApproval                   = 255;
 SN_id_smime_cti_ets_proofOfCreation                    = 'id-smime-cti-ets-proofOfCreation';
 NID_id_smime_cti_ets_proofOfCreation                   = 256;
 SN_ms_csp_name                                         = 'CSPName';
 LN_ms_csp_name                                         = 'Microsoft CSP Name';
 NID_ms_csp_name                                        = 417;
 SN_LocalKeySet                                         = 'LocalKeySet';
 LN_LocalKeySet                                         = 'Microsoft Local Key set';
 NID_LocalKeySet                                        = 856;
 SN_md4                                                 = 'MD4';
 LN_md4                                                 = 'md4';
 NID_md4                                                = 257;
 LN_hmacWithMD5                                         = 'hmacWithMD5';
 NID_hmacWithMD5                                        = 797;
 LN_hmacWithSHA224                                      = 'hmacWithSHA224';
 NID_hmacWithSHA224                                     = 798;
 LN_hmacWithSHA256                                      = 'hmacWithSHA256';
 NID_hmacWithSHA256                                     = 799;
 LN_hmacWithSHA384                                      = 'hmacWithSHA384';
 NID_hmacWithSHA384                                     = 800;
 LN_hmacWithSHA512                                      = 'hmacWithSHA512';
 NID_hmacWithSHA512                                     = 801;
 SN_ms_smartcard_login                                  = 'msSmartcardLogin';
 LN_ms_smartcard_login                                  = 'Microsoft Smartcardlogin';
 NID_ms_smartcard_login                                 = 648;
 SN_ms_upn                                              = 'msUPN';
 LN_ms_upn                                              = 'Microsoft Universal Principal Name';
 NID_ms_upn                                             = 649;
 SN_id_pkix_mod                                         = 'id-pkix-mod';
 NID_id_pkix_mod                                        = 258;
 SN_id_qt                                               = 'id-qt';
 NID_id_qt                                              = 259;
 SN_id_it                                               = 'id-it';
 NID_id_it                                              = 260;
 SN_id_pkip                                             = 'id-pkip';
 NID_id_pkip                                            = 261;
 SN_id_alg                                              = 'id-alg';
 NID_id_alg                                             = 262;
 SN_id_cmc                                              = 'id-cmc';
 NID_id_cmc                                             = 263;
 SN_id_on                                               = 'id-on';
 NID_id_on                                              = 264;
 SN_id_pda                                              = 'id-pda';
 NID_id_pda                                             = 265;
 SN_id_aca                                              = 'id-aca';
 NID_id_aca                                             = 266;
 SN_id_qcs                                              = 'id-qcs';
 NID_id_qcs                                             = 267;
 SN_id_cct                                              = 'id-cct';
 NID_id_cct                                             = 268;
 SN_id_ppl                                              = 'id-ppl';
 NID_id_ppl                                             = 662;
 SN_id_pkix1_explicit_88                                = 'id-pkix1-explicit-88';
 NID_id_pkix1_explicit_88                               = 269;
 SN_id_pkix1_implicit_88                                = 'id-pkix1-implicit-88';
 NID_id_pkix1_implicit_88                               = 270;
 SN_id_pkix1_explicit_93                                = 'id-pkix1-explicit-93';
 NID_id_pkix1_explicit_93                               = 271;
 SN_id_pkix1_implicit_93                                = 'id-pkix1-implicit-93';
 NID_id_pkix1_implicit_93                               = 272;
 SN_id_mod_crmf                                         = 'id-mod-crmf';
 NID_id_mod_crmf                                        = 273;
 SN_id_mod_cmc                                          = 'id-mod-cmc';
 NID_id_mod_cmc                                         = 274;
 SN_id_mod_kea_profile_88                               = 'id-mod-kea-profile-88';
 NID_id_mod_kea_profile_88                              = 275;
 SN_id_mod_kea_profile_93                               = 'id-mod-kea-profile-93';
 NID_id_mod_kea_profile_93                              = 276;
 SN_id_mod_cmp                                          = 'id-mod-cmp';
 NID_id_mod_cmp                                         = 277;
 SN_id_mod_qualified_cert_88                            = 'id-mod-qualified-cert-88';
 NID_id_mod_qualified_cert_88                           = 278;
 SN_id_mod_qualified_cert_93                            = 'id-mod-qualified-cert-93';
 NID_id_mod_qualified_cert_93                           = 279;
 SN_id_mod_attribute_cert                               = 'id-mod-attribute-cert';
 NID_id_mod_attribute_cert                              = 280;
 SN_id_mod_timestamp_protocol                           = 'id-mod-timestamp-protocol';
 NID_id_mod_timestamp_protocol                          = 281;
 SN_id_mod_ocsp                                         = 'id-mod-ocsp';
 NID_id_mod_ocsp                                        = 282;
 SN_id_mod_dvcs                                         = 'id-mod-dvcs';
 NID_id_mod_dvcs                                        = 283;
 SN_id_mod_cmp2000                                      = 'id-mod-cmp2000';
 NID_id_mod_cmp2000                                     = 284;
 SN_biometricInfo                                       = 'biometricInfo';
 LN_biometricInfo                                       = 'Biometric Info';
 NID_biometricInfo                                      = 285;
 SN_qcStatements                                        = 'qcStatements';
 NID_qcStatements                                       = 286;
 SN_ac_auditEntity                                      = 'ac-auditEntity';
 NID_ac_auditEntity                                     = 287;
 SN_ac_targeting                                        = 'ac-targeting';
 NID_ac_targeting                                       = 288;
 SN_aaControls                                          = 'aaControls';
 NID_aaControls                                         = 289;
 SN_sbgp_ipAddrBlock                                    = 'sbgp-ipAddrBlock';
 NID_sbgp_ipAddrBlock                                   = 290;
 SN_sbgp_autonomousSysNum                               = 'sbgp-autonomousSysNum';
 NID_sbgp_autonomousSysNum                              = 291;
 SN_sbgp_routerIdentifier                               = 'sbgp-routerIdentifier';
 NID_sbgp_routerIdentifier                              = 292;
 SN_ac_proxying                                         = 'ac-proxying';
 NID_ac_proxying                                        = 397;
 SN_sinfo_access                                        = 'subjectInfoAccess';
 LN_sinfo_access                                        = 'Subject Information Access';
 NID_sinfo_access                                       = 398;
 SN_proxyCertInfo                                       = 'proxyCertInfo';
 LN_proxyCertInfo                                       = 'Proxy Certificate Information';
 NID_proxyCertInfo                                      = 663;
 SN_textNotice                                          = 'textNotice';
 NID_textNotice                                         = 293;
 SN_ipsecEndSystem                                      = 'ipsecEndSystem';
 LN_ipsecEndSystem                                      = 'IPSec End System';
 NID_ipsecEndSystem                                     = 294;
 SN_ipsecTunnel                                         = 'ipsecTunnel';
 LN_ipsecTunnel                                         = 'IPSec Tunnel';
 NID_ipsecTunnel                                        = 295;
 SN_ipsecUser                                           = 'ipsecUser';
 LN_ipsecUser                                           = 'IPSec User';
 NID_ipsecUser                                          = 296;
 SN_dvcs                                                = 'DVCS';
 LN_dvcs                                                = 'dvcs';
 NID_dvcs                                               = 297;
 SN_id_it_caProtEncCert                                 = 'id-it-caProtEncCert';
 NID_id_it_caProtEncCert                                = 298;
 SN_id_it_signKeyPairTypes                              = 'id-it-signKeyPairTypes';
 NID_id_it_signKeyPairTypes                             = 299;
 SN_id_it_encKeyPairTypes                               = 'id-it-encKeyPairTypes';
 NID_id_it_encKeyPairTypes                              = 300;
 SN_id_it_preferredSymmAlg                              = 'id-it-preferredSymmAlg';
 NID_id_it_preferredSymmAlg                             = 301;
 SN_id_it_caKeyUpdateInfo                               = 'id-it-caKeyUpdateInfo';
 NID_id_it_caKeyUpdateInfo                              = 302;
 SN_id_it_currentCRL                                    = 'id-it-currentCRL';
 NID_id_it_currentCRL                                   = 303;
 SN_id_it_unsupportedOIDs                               = 'id-it-unsupportedOIDs';
 NID_id_it_unsupportedOIDs                              = 304;
 SN_id_it_subscriptionRequest                           = 'id-it-subscriptionRequest';
 NID_id_it_subscriptionRequest                          = 305;
 SN_id_it_subscriptionResponse                          = 'id-it-subscriptionResponse';
 NID_id_it_subscriptionResponse                         = 306;
 SN_id_it_keyPairParamReq                               = 'id-it-keyPairParamReq';
 NID_id_it_keyPairParamReq                              = 307;
 SN_id_it_keyPairParamRep                               = 'id-it-keyPairParamRep';
 NID_id_it_keyPairParamRep                              = 308;
 SN_id_it_revPassphrase                                 = 'id-it-revPassphrase';
 NID_id_it_revPassphrase                                = 309;
 SN_id_it_implicitConfirm                               = 'id-it-implicitConfirm';
 NID_id_it_implicitConfirm                              = 310;
 SN_id_it_confirmWaitTime                               = 'id-it-confirmWaitTime';
 NID_id_it_confirmWaitTime                              = 311;
 SN_id_it_origPKIMessage                                = 'id-it-origPKIMessage';
 NID_id_it_origPKIMessage                               = 312;
 SN_id_it_suppLangTags                                  = 'id-it-suppLangTags';
 NID_id_it_suppLangTags                                 = 784;
 SN_id_regCtrl                                          = 'id-regCtrl';
 NID_id_regCtrl                                         = 313;
 SN_id_regInfo                                          = 'id-regInfo';
 NID_id_regInfo                                         = 314;
 SN_id_regCtrl_regToken                                 = 'id-regCtrl-regToken';
 NID_id_regCtrl_regToken                                = 315;
 SN_id_regCtrl_authenticator                            = 'id-regCtrl-authenticator';
 NID_id_regCtrl_authenticator                           = 316;
 SN_id_regCtrl_pkiPublicationInfo                       = 'id-regCtrl-pkiPublicationInfo';
 NID_id_regCtrl_pkiPublicationInfo                      = 317;
 SN_id_regCtrl_pkiArchiveOptions                        = 'id-regCtrl-pkiArchiveOptions';
 NID_id_regCtrl_pkiArchiveOptions                       = 318;
 SN_id_regCtrl_oldCertID                                = 'id-regCtrl-oldCertID';
 NID_id_regCtrl_oldCertID                               = 319;
 SN_id_regCtrl_protocolEncrKey                          = 'id-regCtrl-protocolEncrKey';
 NID_id_regCtrl_protocolEncrKey                         = 320;
 SN_id_regInfo_utf8Pairs                                = 'id-regInfo-utf8Pairs';
 NID_id_regInfo_utf8Pairs                               = 321;
 SN_id_regInfo_certReq                                  = 'id-regInfo-certReq';
 NID_id_regInfo_certReq                                 = 322;
 SN_id_alg_des40                                        = 'id-alg-des40';
 NID_id_alg_des40                                       = 323;
 SN_id_alg_noSignature                                  = 'id-alg-noSignature';
 NID_id_alg_noSignature                                 = 324;
 SN_id_alg_dh_sig_hmac_sha1                             = 'id-alg-dh-sig-hmac-sha1';
 NID_id_alg_dh_sig_hmac_sha1                            = 325;
 SN_id_alg_dh_pop                                       = 'id-alg-dh-pop';
 NID_id_alg_dh_pop                                      = 326;
 SN_id_cmc_statusInfo                                   = 'id-cmc-statusInfo';
 NID_id_cmc_statusInfo                                  = 327;
 SN_id_cmc_identification                               = 'id-cmc-identification';
 NID_id_cmc_identification                              = 328;
 SN_id_cmc_identityProof                                = 'id-cmc-identityProof';
 NID_id_cmc_identityProof                               = 329;
 SN_id_cmc_dataReturn                                   = 'id-cmc-dataReturn';
 NID_id_cmc_dataReturn                                  = 330;
 SN_id_cmc_transactionId                                = 'id-cmc-transactionId';
 NID_id_cmc_transactionId                               = 331;
 SN_id_cmc_senderNonce                                  = 'id-cmc-senderNonce';
 NID_id_cmc_senderNonce                                 = 332;
 SN_id_cmc_recipientNonce                               = 'id-cmc-recipientNonce';
 NID_id_cmc_recipientNonce                              = 333;
 SN_id_cmc_addExtensions                                = 'id-cmc-addExtensions';
 NID_id_cmc_addExtensions                               = 334;
 SN_id_cmc_encryptedPOP                                 = 'id-cmc-encryptedPOP';
 NID_id_cmc_encryptedPOP                                = 335;
 SN_id_cmc_decryptedPOP                                 = 'id-cmc-decryptedPOP';
 NID_id_cmc_decryptedPOP                                = 336;
 SN_id_cmc_lraPOPWitness                                = 'id-cmc-lraPOPWitness';
 NID_id_cmc_lraPOPWitness                               = 337;
 SN_id_cmc_getCert                                      = 'id-cmc-getCert';
 NID_id_cmc_getCert                                     = 338;
 SN_id_cmc_getCRL                                       = 'id-cmc-getCRL';
 NID_id_cmc_getCRL                                      = 339;
 SN_id_cmc_revokeRequest                                = 'id-cmc-revokeRequest';
 NID_id_cmc_revokeRequest                               = 340;
 SN_id_cmc_regInfo                                      = 'id-cmc-regInfo';
 NID_id_cmc_regInfo                                     = 341;
 SN_id_cmc_responseInfo                                 = 'id-cmc-responseInfo';
 NID_id_cmc_responseInfo                                = 342;
 SN_id_cmc_queryPending                                 = 'id-cmc-queryPending';
 NID_id_cmc_queryPending                                = 343;
 SN_id_cmc_popLinkRandom                                = 'id-cmc-popLinkRandom';
 NID_id_cmc_popLinkRandom                               = 344;
 SN_id_cmc_popLinkWitness                               = 'id-cmc-popLinkWitness';
 NID_id_cmc_popLinkWitness                              = 345;
 SN_id_cmc_confirmCertAcceptance                        = 'id-cmc-confirmCertAcceptance';
 NID_id_cmc_confirmCertAcceptance                       = 346;
 SN_id_on_personalData                                  = 'id-on-personalData';
 NID_id_on_personalData                                 = 347;
 SN_id_on_permanentIdentifier                           = 'id-on-permanentIdentifier';
 LN_id_on_permanentIdentifier                           = 'Permanent Identifier';
 NID_id_on_permanentIdentifier                          = 858;
 SN_id_pda_dateOfBirth                                  = 'id-pda-dateOfBirth';
 NID_id_pda_dateOfBirth                                 = 348;
 SN_id_pda_placeOfBirth                                 = 'id-pda-placeOfBirth';
 NID_id_pda_placeOfBirth                                = 349;
 SN_id_pda_gender                                       = 'id-pda-gender';
 NID_id_pda_gender                                      = 351;
 SN_id_pda_countryOfCitizenship                         = 'id-pda-countryOfCitizenship';
 NID_id_pda_countryOfCitizenship                        = 352;
 SN_id_pda_countryOfResidence                           = 'id-pda-countryOfResidence';
 NID_id_pda_countryOfResidence                          = 353;
 SN_id_aca_authenticationInfo                           = 'id-aca-authenticationInfo';
 NID_id_aca_authenticationInfo                          = 354;
 SN_id_aca_accessIdentity                               = 'id-aca-accessIdentity';
 NID_id_aca_accessIdentity                              = 355;
 SN_id_aca_chargingIdentity                             = 'id-aca-chargingIdentity';
 NID_id_aca_chargingIdentity                            = 356;
 SN_id_aca_group                                        = 'id-aca-group';
 NID_id_aca_group                                       = 357;
 SN_id_aca_role                                         = 'id-aca-role';
 NID_id_aca_role                                        = 358;
 SN_id_aca_encAttrs                                     = 'id-aca-encAttrs';
 NID_id_aca_encAttrs                                    = 399;
 SN_id_qcs_pkixQCSyntax_v1                              = 'id-qcs-pkixQCSyntax-v1';
 NID_id_qcs_pkixQCSyntax_v1                             = 359;
 SN_id_cct_crs                                          = 'id-cct-crs';
 NID_id_cct_crs                                         = 360;
 SN_id_cct_PKIData                                      = 'id-cct-PKIData';
 NID_id_cct_PKIData                                     = 361;
 SN_id_cct_PKIResponse                                  = 'id-cct-PKIResponse';
 NID_id_cct_PKIResponse                                 = 362;
 SN_id_ppl_anyLanguage                                  = 'id-ppl-anyLanguage';
 LN_id_ppl_anyLanguage                                  = 'Any language';
 NID_id_ppl_anyLanguage                                 = 664;
 SN_id_ppl_inheritAll                                   = 'id-ppl-inheritAll';
 LN_id_ppl_inheritAll                                   = 'Inherit all';
 NID_id_ppl_inheritAll                                  = 665;
 SN_Independent                                         = 'id-ppl-independent';
 LN_Independent                                         = 'Independent';
 NID_Independent                                        = 667;
 SN_ad_timeStamping                                     = 'ad_timestamping';
 LN_ad_timeStamping                                     = 'AD Time Stamping';
 NID_ad_timeStamping                                    = 363;
 SN_ad_dvcs                                             = 'AD_DVCS';
 LN_ad_dvcs                                             = 'ad dvcs';
 NID_ad_dvcs                                            = 364;
 SN_caRepository                                        = 'caRepository';
 LN_caRepository                                        = 'CA Repository';
 NID_caRepository                                       = 785;
 SN_id_pkix_OCSP_basic                                  = 'basicOCSPResponse';
 LN_id_pkix_OCSP_basic                                  = 'Basic OCSP Response';
 NID_id_pkix_OCSP_basic                                 = 365;
 SN_id_pkix_OCSP_Nonce                                  = 'Nonce';
 LN_id_pkix_OCSP_Nonce                                  = 'OCSP Nonce';
 NID_id_pkix_OCSP_Nonce                                 = 366;
 SN_id_pkix_OCSP_CrlID                                  = 'CrlID';
 LN_id_pkix_OCSP_CrlID                                  = 'OCSP CRL ID';
 NID_id_pkix_OCSP_CrlID                                 = 367;
 SN_id_pkix_OCSP_acceptableResponses                    = 'acceptableResponses';
 LN_id_pkix_OCSP_acceptableResponses                    = 'Acceptable OCSP Responses';
 NID_id_pkix_OCSP_acceptableResponses                   = 368;
 SN_id_pkix_OCSP_noCheck                                = 'noCheck';
 LN_id_pkix_OCSP_noCheck                                = 'OCSP No Check';
 NID_id_pkix_OCSP_noCheck                               = 369;
 SN_id_pkix_OCSP_archiveCutoff                          = 'archiveCutoff';
 LN_id_pkix_OCSP_archiveCutoff                          = 'OCSP Archive Cutoff';
 NID_id_pkix_OCSP_archiveCutoff                         = 370;
 SN_id_pkix_OCSP_serviceLocator                         = 'serviceLocator';
 LN_id_pkix_OCSP_serviceLocator                         = 'OCSP Service Locator';
 NID_id_pkix_OCSP_serviceLocator                        = 371;
 SN_id_pkix_OCSP_extendedStatus                         = 'extendedStatus';
 LN_id_pkix_OCSP_extendedStatus                         = 'Extended OCSP Status';
 NID_id_pkix_OCSP_extendedStatus                        = 372;
 SN_id_pkix_OCSP_valid                                  = 'valid';
 NID_id_pkix_OCSP_valid                                 = 373;
 SN_id_pkix_OCSP_path                                   = 'path';
 NID_id_pkix_OCSP_path                                  = 374;
 SN_id_pkix_OCSP_trustRoot                              = 'trustRoot';
 LN_id_pkix_OCSP_trustRoot                              = 'Trust Root';
 NID_id_pkix_OCSP_trustRoot                             = 375;
 SN_rsaSignature                                        = 'rsaSignature';
 NID_rsaSignature                                       = 377;
 SN_des_ede_ecb                                         = 'DES-EDE';
 LN_des_ede_ecb                                         = 'des-ede';
 NID_des_ede_ecb                                        = 32;
 SN_des_ede3_ecb                                        = 'DES-EDE3';
 LN_des_ede3_ecb                                        = 'des-ede3';
 NID_des_ede3_ecb                                       = 33;
 SN_streetAddress                                       = 'street';
 LN_streetAddress                                       = 'streetAddress';
 NID_streetAddress                                      = 660;
 LN_searchGuide                                         = 'searchGuide';
 NID_searchGuide                                        = 859;
 LN_businessCategory                                    = 'businessCategory';
 NID_businessCategory                                   = 860;
 LN_postalAddress                                       = 'postalAddress';
 NID_postalAddress                                      = 861;
 LN_postalCode                                          = 'postalCode';
 NID_postalCode                                         = 661;
 LN_postOfficeBox                                       = 'postOfficeBox';
 NID_postOfficeBox                                      = 862;
 LN_physicalDeliveryOfficeName                          = 'physicalDeliveryOfficeName';
 NID_physicalDeliveryOfficeName                         = 863;
 LN_telephoneNumber                                     = 'telephoneNumber';
 NID_telephoneNumber                                    = 864;
 LN_telexNumber                                         = 'telexNumber';
 NID_telexNumber                                        = 865;
 LN_teletexTerminalIdentifier                           = 'teletexTerminalIdentifier';
 NID_teletexTerminalIdentifier                          = 866;
 LN_facsimileTelephoneNumber                            = 'facsimileTelephoneNumber';
 NID_facsimileTelephoneNumber                           = 867;
 LN_x121Address                                         = 'x121Address';
 NID_x121Address                                        = 868;
 LN_internationaliSDNNumber                             = 'internationaliSDNNumber';
 NID_internationaliSDNNumber                            = 869;
 LN_registeredAddress                                   = 'registeredAddress';
 NID_registeredAddress                                  = 870;
 LN_destinationIndicator                                = 'destinationIndicator';
 NID_destinationIndicator                               = 871;
 LN_preferredDeliveryMethod                             = 'preferredDeliveryMethod';
 NID_preferredDeliveryMethod                            = 872;
 LN_presentationAddress                                 = 'presentationAddress';
 NID_presentationAddress                                = 873;
 LN_supportedApplicationContext                         = 'supportedApplicationContext';
 NID_supportedApplicationContext                        = 874;
 SN_member                                              = 'member';
 NID_member                                             = 875;
 SN_owner                                               = 'owner';
 NID_owner                                              = 876;
 LN_roleOccupant                                        = 'roleOccupant';
 NID_roleOccupant                                       = 877;
 SN_seeAlso                                             = 'seeAlso';
 NID_seeAlso                                            = 878;
 LN_userPassword                                        = 'userPassword';
 NID_userPassword                                       = 879;
 LN_userCertificate                                     = 'userCertificate';
 NID_userCertificate                                    = 880;
 LN_cACertificate                                       = 'cACertificate';
 NID_cACertificate                                      = 881;
 LN_authorityRevocationList                             = 'authorityRevocationList';
 NID_authorityRevocationList                            = 882;
 LN_certificateRevocationList                           = 'certificateRevocationList';
 NID_certificateRevocationList                          = 883;
 LN_crossCertificatePair                                = 'crossCertificatePair';
 NID_crossCertificatePair                               = 884;
 LN_generationQualifier                                 = 'generationQualifier';
 NID_generationQualifier                                = 509;
 LN_x500UniqueIdentifier                                = 'x500UniqueIdentifier';
 NID_x500UniqueIdentifier                               = 503;
 LN_enhancedSearchGuide                                 = 'enhancedSearchGuide';
 NID_enhancedSearchGuide                                = 885;
 LN_protocolInformation                                 = 'protocolInformation';
 NID_protocolInformation                                = 886;
 LN_distinguishedName                                   = 'distinguishedName';
 NID_distinguishedName                                  = 887;
 LN_uniqueMember                                        = 'uniqueMember';
 NID_uniqueMember                                       = 888;
 LN_houseIdentifier                                     = 'houseIdentifier';
 NID_houseIdentifier                                    = 889;
 LN_supportedAlgorithms                                 = 'supportedAlgorithms';
 NID_supportedAlgorithms                                = 890;
 LN_deltaRevocationList                                 = 'deltaRevocationList';
 NID_deltaRevocationList                                = 891;
 SN_dmdName                                             = 'dmdName';
 NID_dmdName                                            = 892;
 LN_pseudonym                                           = 'pseudonym';
 NID_pseudonym                                          = 510;
 SN_role                                                = 'role';
 LN_role                                                = 'role';
 NID_role                                               = 400;
 SN_X500algorithms                                      = 'X500algorithms';
 LN_X500algorithms                                      = 'directory services - algorithms';
 NID_X500algorithms                                     = 378;
 SN_subject_directory_attributes                        = 'subjectDirectoryAttributes';
 LN_subject_directory_attributes                        = 'X509v3 Subject Directory Attributes';
 NID_subject_directory_attributes                       = 769;
 SN_issuing_distribution_point                          = 'issuingDistributionPoint';
 LN_issuing_distribution_point                          = 'X509v3 Issuing Distrubution Point';
 NID_issuing_distribution_point                         = 770;
 SN_certificate_issuer                                  = 'certificateIssuer';
 LN_certificate_issuer                                  = 'X509v3 Certificate Issuer';
 NID_certificate_issuer                                 = 771;
 SN_name_constraints                                    = 'nameConstraints';
 LN_name_constraints                                    = 'X509v3 Name Constraints';
 NID_name_constraints                                   = 666;
 SN_any_policy                                          = 'anyPolicy';
 LN_any_policy                                          = 'X509v3 Any Policy';
 NID_any_policy                                         = 746;
 SN_policy_mappings                                     = 'policyMappings';
 LN_policy_mappings                                     = 'X509v3 Policy Mappings';
 NID_policy_mappings                                    = 747;
 SN_policy_constraints                                  = 'policyConstraints';
 LN_policy_constraints                                  = 'X509v3 Policy Constraints';
 NID_policy_constraints                                 = 401;
 SN_freshest_crl                                        = 'freshestCRL';
 LN_freshest_crl                                        = 'X509v3 Freshest CRL';
 NID_freshest_crl                                       = 857;
 SN_inhibit_any_policy                                  = 'inhibitAnyPolicy';
 LN_inhibit_any_policy                                  = 'X509v3 Inhibit Any Policy';
 NID_inhibit_any_policy                                 = 748;
 SN_target_information                                  = 'targetInformation';
 LN_target_information                                  = 'X509v3 AC Targeting';
 NID_target_information                                 = 402;
 SN_no_rev_avail                                        = 'noRevAvail';
 LN_no_rev_avail                                        = 'X509v3 No Revocation Available';
 NID_no_rev_avail                                       = 403;
 SN_anyExtendedKeyUsage                                 = 'anyExtendedKeyUsage';
 LN_anyExtendedKeyUsage                                 = 'Any Extended Key Usage';
 NID_anyExtendedKeyUsage                                = 910;
 SN_org                                                 = 'ORG';
 LN_org                                                 = 'org';
 NID_org                                                = 379;
 SN_dod                                                 = 'DOD';
 LN_dod                                                 = 'dod';
 NID_dod                                                = 380;
 SN_iana                                                = 'IANA';
 LN_iana                                                = 'iana';
 NID_iana                                               = 381;
 SN_Directory                                           = 'directory';
 LN_Directory                                           = 'Directory';
 NID_Directory                                          = 382;
 SN_Management                                          = 'mgmt';
 LN_Management                                          = 'Management';
 NID_Management                                         = 383;
 SN_Experimental                                        = 'experimental';
 LN_Experimental                                        = 'Experimental';
 NID_Experimental                                       = 384;
 SN_Private                                             = 'private';
 LN_Private                                             = 'Private';
 NID_Private                                            = 385;
 SN_Security                                            = 'security';
 LN_Security                                            = 'Security';
 NID_Security                                           = 386;
 SN_SNMPv2                                              = 'snmpv2';
 LN_SNMPv2                                              = 'SNMPv2';
 NID_SNMPv2                                             = 387;
 LN_Mail                                                = 'Mail';
 NID_Mail                                               = 388;
 SN_Enterprises                                         = 'enterprises';
 LN_Enterprises                                         = 'Enterprises';
 NID_Enterprises                                        = 389;
 SN_dcObject                                            = 'dcobject';
 LN_dcObject                                            = 'dcObject';
 NID_dcObject                                           = 390;
 SN_mime_mhs                                            = 'mime-mhs';
 LN_mime_mhs                                            = 'MIME MHS';
 NID_mime_mhs                                           = 504;
 SN_mime_mhs_headings                                   = 'mime-mhs-headings';
 LN_mime_mhs_headings                                   = 'mime-mhs-headings';
 NID_mime_mhs_headings                                  = 505;
 SN_mime_mhs_bodies                                     = 'mime-mhs-bodies';
 LN_mime_mhs_bodies                                     = 'mime-mhs-bodies';
 NID_mime_mhs_bodies                                    = 506;
 SN_id_hex_partial_message                              = 'id-hex-partial-message';
 LN_id_hex_partial_message                              = 'id-hex-partial-message';
 NID_id_hex_partial_message                             = 507;
 SN_id_hex_multipart_message                            = 'id-hex-multipart-message';
 LN_id_hex_multipart_message                            = 'id-hex-multipart-message';
 NID_id_hex_multipart_message                           = 508;
 SN_aes_128_ecb                                         = 'AES-128-ECB';
 LN_aes_128_ecb                                         = 'aes-128-ecb';
 NID_aes_128_ecb                                        = 418;
 SN_aes_128_cbc                                         = 'AES-128-CBC';
 LN_aes_128_cbc                                         = 'aes-128-cbc';
 NID_aes_128_cbc                                        = 419;
 SN_aes_128_ofb128                                      = 'AES-128-OFB';
 LN_aes_128_ofb128                                      = 'aes-128-ofb';
 NID_aes_128_ofb128                                     = 420;
 SN_aes_128_cfb128                                      = 'AES-128-CFB';
 LN_aes_128_cfb128                                      = 'aes-128-cfb';
 NID_aes_128_cfb128                                     = 421;
 SN_id_aes128_wrap                                      = 'id-aes128-wrap';
 NID_id_aes128_wrap                                     = 788;
 SN_aes_128_gcm                                         = 'id-aes128-GCM';
 LN_aes_128_gcm                                         = 'aes-128-gcm';
 NID_aes_128_gcm                                        = 895;
 SN_aes_128_ccm                                         = 'id-aes128-CCM';
 LN_aes_128_ccm                                         = 'aes-128-ccm';
 NID_aes_128_ccm                                        = 896;
 SN_id_aes128_wrap_pad                                  = 'id-aes128-wrap-pad';
 NID_id_aes128_wrap_pad                                 = 897;
 SN_aes_192_ecb                                         = 'AES-192-ECB';
 LN_aes_192_ecb                                         = 'aes-192-ecb';
 NID_aes_192_ecb                                        = 422;
 SN_aes_192_cbc                                         = 'AES-192-CBC';
 LN_aes_192_cbc                                         = 'aes-192-cbc';
 NID_aes_192_cbc                                        = 423;
 SN_aes_192_ofb128                                      = 'AES-192-OFB';
 LN_aes_192_ofb128                                      = 'aes-192-ofb';
 NID_aes_192_ofb128                                     = 424;
 SN_aes_192_cfb128                                      = 'AES-192-CFB';
 LN_aes_192_cfb128                                      = 'aes-192-cfb';
 NID_aes_192_cfb128                                     = 425;
 SN_id_aes192_wrap                                      = 'id-aes192-wrap';
 NID_id_aes192_wrap                                     = 789;
 SN_aes_192_gcm                                         = 'id-aes192-GCM';
 LN_aes_192_gcm                                         = 'aes-192-gcm';
 NID_aes_192_gcm                                        = 898;
 SN_aes_192_ccm                                         = 'id-aes192-CCM';
 LN_aes_192_ccm                                         = 'aes-192-ccm';
 NID_aes_192_ccm                                        = 899;
 SN_id_aes192_wrap_pad                                  = 'id-aes192-wrap-pad';
 NID_id_aes192_wrap_pad                                 = 900;
 SN_aes_256_ecb                                         = 'AES-256-ECB';
 LN_aes_256_ecb                                         = 'aes-256-ecb';
 NID_aes_256_ecb                                        = 426;
 SN_aes_256_cbc                                         = 'AES-256-CBC';
 LN_aes_256_cbc                                         = 'aes-256-cbc';
 NID_aes_256_cbc                                        = 427;
 SN_aes_256_ofb128                                      = 'AES-256-OFB';
 LN_aes_256_ofb128                                      = 'aes-256-ofb';
 NID_aes_256_ofb128                                     = 428;
 SN_aes_256_cfb128                                      = 'AES-256-CFB';
 LN_aes_256_cfb128                                      = 'aes-256-cfb';
 NID_aes_256_cfb128                                     = 429;
 SN_id_aes256_wrap                                      = 'id-aes256-wrap';
 NID_id_aes256_wrap                                     = 790;
 SN_aes_256_gcm                                         = 'id-aes256-GCM';
 LN_aes_256_gcm                                         = 'aes-256-gcm';
 NID_aes_256_gcm                                        = 901;
 SN_aes_256_ccm                                         = 'id-aes256-CCM';
 LN_aes_256_ccm                                         = 'aes-256-ccm';
 NID_aes_256_ccm                                        = 902;
 SN_id_aes256_wrap_pad                                  = 'id-aes256-wrap-pad';
 NID_id_aes256_wrap_pad                                 = 903;
 SN_aes_128_cfb1                                        = 'AES-128-CFB1';
 LN_aes_128_cfb1                                        = 'aes-128-cfb1';
 NID_aes_128_cfb1                                       = 650;
 SN_aes_192_cfb1                                        = 'AES-192-CFB1';
 LN_aes_192_cfb1                                        = 'aes-192-cfb1';
 NID_aes_192_cfb1                                       = 651;
 SN_aes_256_cfb1                                        = 'AES-256-CFB1';
 LN_aes_256_cfb1                                        = 'aes-256-cfb1';
 NID_aes_256_cfb1                                       = 652;
 SN_aes_128_cfb8                                        = 'AES-128-CFB8';
 LN_aes_128_cfb8                                        = 'aes-128-cfb8';
 NID_aes_128_cfb8                                       = 653;
 SN_aes_192_cfb8                                        = 'AES-192-CFB8';
 LN_aes_192_cfb8                                        = 'aes-192-cfb8';
 NID_aes_192_cfb8                                       = 654;
 SN_aes_256_cfb8                                        = 'AES-256-CFB8';
 LN_aes_256_cfb8                                        = 'aes-256-cfb8';
 NID_aes_256_cfb8                                       = 655;
 SN_aes_128_ctr                                         = 'AES-128-CTR';
 LN_aes_128_ctr                                         = 'aes-128-ctr';
 NID_aes_128_ctr                                        = 904;
 SN_aes_192_ctr                                         = 'AES-192-CTR';
 LN_aes_192_ctr                                         = 'aes-192-ctr';
 NID_aes_192_ctr                                        = 905;
 SN_aes_256_ctr                                         = 'AES-256-CTR';
 LN_aes_256_ctr                                         = 'aes-256-ctr';
 NID_aes_256_ctr                                        = 906;
 SN_aes_128_xts                                         = 'AES-128-XTS';
 LN_aes_128_xts                                         = 'aes-128-xts';
 NID_aes_128_xts                                        = 913;
 SN_aes_256_xts                                         = 'AES-256-XTS';
 LN_aes_256_xts                                         = 'aes-256-xts';
 NID_aes_256_xts                                        = 914;
 SN_des_cfb1                                            = 'DES-CFB1';
 LN_des_cfb1                                            = 'des-cfb1';
 NID_des_cfb1                                           = 656;
 SN_des_cfb8                                            = 'DES-CFB8';
 LN_des_cfb8                                            = 'des-cfb8';
 NID_des_cfb8                                           = 657;
 SN_des_ede3_cfb1                                       = 'DES-EDE3-CFB1';
 LN_des_ede3_cfb1                                       = 'des-ede3-cfb1';
 NID_des_ede3_cfb1                                      = 658;
 SN_des_ede3_cfb8                                       = 'DES-EDE3-CFB8';
 LN_des_ede3_cfb8                                       = 'des-ede3-cfb8';
 NID_des_ede3_cfb8                                      = 659;
 SN_sha256                                              = 'SHA256';
 LN_sha256                                              = 'sha256';
 NID_sha256                                             = 672;
 SN_sha384                                              = 'SHA384';
 LN_sha384                                              = 'sha384';
 NID_sha384                                             = 673;
 SN_sha512                                              = 'SHA512';
 LN_sha512                                              = 'sha512';
 NID_sha512                                             = 674;
 SN_sha224                                              = 'SHA224';
 LN_sha224                                              = 'sha224';
 NID_sha224                                             = 675;
 SN_dsa_with_SHA224                                     = 'dsa_with_SHA224';
 NID_dsa_with_SHA224                                    = 802;
 SN_dsa_with_SHA256                                     = 'dsa_with_SHA256';
 NID_dsa_with_SHA256                                    = 803;
 SN_hold_instruction_code                               = 'holdInstructionCode';
 LN_hold_instruction_code                               = 'Hold Instruction Code';
 NID_hold_instruction_code                              = 430;
 SN_hold_instruction_none                               = 'holdInstructionNone';
 LN_hold_instruction_none                               = 'Hold Instruction None';
 NID_hold_instruction_none                              = 431;
 SN_hold_instruction_call_issuer                        = 'holdInstructionCallIssuer';
 LN_hold_instruction_call_issuer                        = 'Hold Instruction Call Issuer';
 NID_hold_instruction_call_issuer                       = 432;
 SN_hold_instruction_reject                             = 'holdInstructionReject';
 LN_hold_instruction_reject                             = 'Hold Instruction Reject';
 NID_hold_instruction_reject                            = 433;
 SN_data                                                = 'data';
 NID_data                                               = 434;
 SN_pss                                                 = 'pss';
 NID_pss                                                = 435;
 SN_ucl                                                 = 'ucl';
 NID_ucl                                                = 436;
 SN_pilot                                               = 'pilot';
 NID_pilot                                              = 437;
 LN_pilotAttributeType                                  = 'pilotAttributeType';
 NID_pilotAttributeType                                 = 438;
 LN_pilotAttributeSyntax                                = 'pilotAttributeSyntax';
 NID_pilotAttributeSyntax                               = 439;
 LN_pilotObjectClass                                    = 'pilotObjectClass';
 NID_pilotObjectClass                                   = 440;
 LN_pilotGroups                                         = 'pilotGroups';
 NID_pilotGroups                                        = 441;
 LN_iA5StringSyntax                                     = 'iA5StringSyntax';
 NID_iA5StringSyntax                                    = 442;
 LN_caseIgnoreIA5StringSyntax                           = 'caseIgnoreIA5StringSyntax';
 NID_caseIgnoreIA5StringSyntax                          = 443;
 LN_pilotObject                                         = 'pilotObject';
 NID_pilotObject                                        = 444;
 LN_pilotPerson                                         = 'pilotPerson';
 NID_pilotPerson                                        = 445;
 SN_account                                             = 'account';
 NID_account                                            = 446;
 SN_document                                            = 'document';
 NID_document                                           = 447;
 SN_room                                                = 'room';
 NID_room                                               = 448;
 LN_documentSeries                                      = 'documentSeries';
 NID_documentSeries                                     = 449;
 SN_Domain                                              = 'domain';
 LN_Domain                                              = 'Domain';
 NID_Domain                                             = 392;
 LN_rFC822localPart                                     = 'rFC822localPart';
 NID_rFC822localPart                                    = 450;
 LN_dNSDomain                                           = 'dNSDomain';
 NID_dNSDomain                                          = 451;
 LN_domainRelatedObject                                 = 'domainRelatedObject';
 NID_domainRelatedObject                                = 452;
 LN_friendlyCountry                                     = 'friendlyCountry';
 NID_friendlyCountry                                    = 453;
 LN_simpleSecurityObject                                = 'simpleSecurityObject';
 NID_simpleSecurityObject                               = 454;
 LN_pilotOrganization                                   = 'pilotOrganization';
 NID_pilotOrganization                                  = 455;
 LN_pilotDSA                                            = 'pilotDSA';
 NID_pilotDSA                                           = 456;
 LN_qualityLabelledData                                 = 'qualityLabelledData';
 NID_qualityLabelledData                                = 457;
 SN_userId                                              = 'UID';
 LN_userId                                              = 'userId';
 NID_userId                                             = 458;
 LN_textEncodedORAddress                                = 'textEncodedORAddress';
 NID_textEncodedORAddress                               = 459;
 SN_rfc822Mailbox                                       = 'mail';
 LN_rfc822Mailbox                                       = 'rfc822Mailbox';
 NID_rfc822Mailbox                                      = 460;
 SN_info                                                = 'info';
 NID_info                                               = 461;
 LN_favouriteDrink                                      = 'favouriteDrink';
 NID_favouriteDrink                                     = 462;
 LN_roomNumber                                          = 'roomNumber';
 NID_roomNumber                                         = 463;
 SN_photo                                               = 'photo';
 NID_photo                                              = 464;
 LN_userClass                                           = 'userClass';
 NID_userClass                                          = 465;
 SN_host                                                = 'host';
 NID_host                                               = 466;
 SN_manager                                             = 'manager';
 NID_manager                                            = 467;
 LN_documentIdentifier                                  = 'documentIdentifier';
 NID_documentIdentifier                                 = 468;
 LN_documentTitle                                       = 'documentTitle';
 NID_documentTitle                                      = 469;
 LN_documentVersion                                     = 'documentVersion';
 NID_documentVersion                                    = 470;
 LN_documentAuthor                                      = 'documentAuthor';
 NID_documentAuthor                                     = 471;
 LN_documentLocation                                    = 'documentLocation';
 NID_documentLocation                                   = 472;
 LN_homeTelephoneNumber                                 = 'homeTelephoneNumber';
 NID_homeTelephoneNumber                                = 473;
 SN_secretary                                           = 'secretary';
 NID_secretary                                          = 474;
 LN_otherMailbox                                        = 'otherMailbox';
 NID_otherMailbox                                       = 475;
 LN_lastModifiedTime                                    = 'lastModifiedTime';
 NID_lastModifiedTime                                   = 476;
 LN_lastModifiedBy                                      = 'lastModifiedBy';
 NID_lastModifiedBy                                     = 477;
 SN_domainComponent                                     = 'DC';
 LN_domainComponent                                     = 'domainComponent';
 NID_domainComponent                                    = 391;
 LN_aRecord                                             = 'aRecord';
 NID_aRecord                                            = 478;
 LN_pilotAttributeType27                                = 'pilotAttributeType27';
 NID_pilotAttributeType27                               = 479;
 LN_mXRecord                                            = 'mXRecord';
 NID_mXRecord                                           = 480;
 LN_nSRecord                                            = 'nSRecord';
 NID_nSRecord                                           = 481;
 LN_sOARecord                                           = 'sOARecord';
 NID_sOARecord                                          = 482;
 LN_cNAMERecord                                         = 'cNAMERecord';
 NID_cNAMERecord                                        = 483;
 LN_associatedDomain                                    = 'associatedDomain';
 NID_associatedDomain                                   = 484;
 LN_associatedName                                      = 'associatedName';
 NID_associatedName                                     = 485;
 LN_homePostalAddress                                   = 'homePostalAddress';
 NID_homePostalAddress                                  = 486;
 LN_personalTitle                                       = 'personalTitle';
 NID_personalTitle                                      = 487;
 LN_mobileTelephoneNumber                               = 'mobileTelephoneNumber';
 NID_mobileTelephoneNumber                              = 488;
 LN_pagerTelephoneNumber                                = 'pagerTelephoneNumber';
 NID_pagerTelephoneNumber                               = 489;
 LN_friendlyCountryName                                 = 'friendlyCountryName';
 NID_friendlyCountryName                                = 490;
 LN_organizationalStatus                                = 'organizationalStatus';
 NID_organizationalStatus                               = 491;
 LN_janetMailbox                                        = 'janetMailbox';
 NID_janetMailbox                                       = 492;
 LN_mailPreferenceOption                                = 'mailPreferenceOption';
 NID_mailPreferenceOption                               = 493;
 LN_buildingName                                        = 'buildingName';
 NID_buildingName                                       = 494;
 LN_dSAQuality                                          = 'dSAQuality';
 NID_dSAQuality                                         = 495;
 LN_singleLevelQuality                                  = 'singleLevelQuality';
 NID_singleLevelQuality                                 = 496;
 LN_subtreeMinimumQuality                               = 'subtreeMinimumQuality';
 NID_subtreeMinimumQuality                              = 497;
 LN_subtreeMaximumQuality                               = 'subtreeMaximumQuality';
 NID_subtreeMaximumQuality                              = 498;
 LN_personalSignature                                   = 'personalSignature';
 NID_personalSignature                                  = 499;
 LN_dITRedirect                                         = 'dITRedirect';
 NID_dITRedirect                                        = 500;
 SN_audio                                               = 'audio';
 NID_audio                                              = 501;
 LN_documentPublisher                                   = 'documentPublisher';
 NID_documentPublisher                                  = 502;
 SN_id_set                                              = 'id-set';
 LN_id_set                                              = 'Secure Electronic Transactions';
 NID_id_set                                             = 512;
 SN_set_ctype                                           = 'set-ctype';
 LN_set_ctype                                           = 'content types';
 NID_set_ctype                                          = 513;
 SN_set_msgExt                                          = 'set-msgExt';
 LN_set_msgExt                                          = 'message extensions';
 NID_set_msgExt                                         = 514;
 SN_set_attr                                            = 'set-attr';
 NID_set_attr                                           = 515;
 SN_set_policy                                          = 'set-policy';
 NID_set_policy                                         = 516;
 SN_set_certExt                                         = 'set-certExt';
 LN_set_certExt                                         = 'certificate extensions';
 NID_set_certExt                                        = 517;
 SN_set_brand                                           = 'set-brand';
 NID_set_brand                                          = 518;
 SN_setct_PANData                                       = 'setct-PANData';
 NID_setct_PANData                                      = 519;
 SN_setct_PANToken                                      = 'setct-PANToken';
 NID_setct_PANToken                                     = 520;
 SN_setct_PANOnly                                       = 'setct-PANOnly';
 NID_setct_PANOnly                                      = 521;
 SN_setct_OIData                                        = 'setct-OIData';
 NID_setct_OIData                                       = 522;
 SN_setct_PI                                            = 'setct-PI';
 NID_setct_PI                                           = 523;
 SN_setct_PIData                                        = 'setct-PIData';
 NID_setct_PIData                                       = 524;
 SN_setct_PIDataUnsigned                                = 'setct-PIDataUnsigned';
 NID_setct_PIDataUnsigned                               = 525;
 SN_setct_HODInput                                      = 'setct-HODInput';
 NID_setct_HODInput                                     = 526;
 SN_setct_AuthResBaggage                                = 'setct-AuthResBaggage';
 NID_setct_AuthResBaggage                               = 527;
 SN_setct_AuthRevReqBaggage                             = 'setct-AuthRevReqBaggage';
 NID_setct_AuthRevReqBaggage                            = 528;
 SN_setct_AuthRevResBaggage                             = 'setct-AuthRevResBaggage';
 NID_setct_AuthRevResBaggage                            = 529;
 SN_setct_CapTokenSeq                                   = 'setct-CapTokenSeq';
 NID_setct_CapTokenSeq                                  = 530;
 SN_setct_PInitResData                                  = 'setct-PInitResData';
 NID_setct_PInitResData                                 = 531;
 SN_setct_PI_TBS                                        = 'setct-PI-TBS';
 NID_setct_PI_TBS                                       = 532;
 SN_setct_PResData                                      = 'setct-PResData';
 NID_setct_PResData                                     = 533;
 SN_setct_AuthReqTBS                                    = 'setct-AuthReqTBS';
 NID_setct_AuthReqTBS                                   = 534;
 SN_setct_AuthResTBS                                    = 'setct-AuthResTBS';
 NID_setct_AuthResTBS                                   = 535;
 SN_setct_AuthResTBSX                                   = 'setct-AuthResTBSX';
 NID_setct_AuthResTBSX                                  = 536;
 SN_setct_AuthTokenTBS                                  = 'setct-AuthTokenTBS';
 NID_setct_AuthTokenTBS                                 = 537;
 SN_setct_CapTokenData                                  = 'setct-CapTokenData';
 NID_setct_CapTokenData                                 = 538;
 SN_setct_CapTokenTBS                                   = 'setct-CapTokenTBS';
 NID_setct_CapTokenTBS                                  = 539;
 SN_setct_AcqCardCodeMsg                                = 'setct-AcqCardCodeMsg';
 NID_setct_AcqCardCodeMsg                               = 540;
 SN_setct_AuthRevReqTBS                                 = 'setct-AuthRevReqTBS';
 NID_setct_AuthRevReqTBS                                = 541;
 SN_setct_AuthRevResData                                = 'setct-AuthRevResData';
 NID_setct_AuthRevResData                               = 542;
 SN_setct_AuthRevResTBS                                 = 'setct-AuthRevResTBS';
 NID_setct_AuthRevResTBS                                = 543;
 SN_setct_CapReqTBS                                     = 'setct-CapReqTBS';
 NID_setct_CapReqTBS                                    = 544;
 SN_setct_CapReqTBSX                                    = 'setct-CapReqTBSX';
 NID_setct_CapReqTBSX                                   = 545;
 SN_setct_CapResData                                    = 'setct-CapResData';
 NID_setct_CapResData                                   = 546;
 SN_setct_CapRevReqTBS                                  = 'setct-CapRevReqTBS';
 NID_setct_CapRevReqTBS                                 = 547;
 SN_setct_CapRevReqTBSX                                 = 'setct-CapRevReqTBSX';
 NID_setct_CapRevReqTBSX                                = 548;
 SN_setct_CapRevResData                                 = 'setct-CapRevResData';
 NID_setct_CapRevResData                                = 549;
 SN_setct_CredReqTBS                                    = 'setct-CredReqTBS';
 NID_setct_CredReqTBS                                   = 550;
 SN_setct_CredReqTBSX                                   = 'setct-CredReqTBSX';
 NID_setct_CredReqTBSX                                  = 551;
 SN_setct_CredResData                                   = 'setct-CredResData';
 NID_setct_CredResData                                  = 552;
 SN_setct_CredRevReqTBS                                 = 'setct-CredRevReqTBS';
 NID_setct_CredRevReqTBS                                = 553;
 SN_setct_CredRevReqTBSX                                = 'setct-CredRevReqTBSX';
 NID_setct_CredRevReqTBSX                               = 554;
 SN_setct_CredRevResData                                = 'setct-CredRevResData';
 NID_setct_CredRevResData                               = 555;
 SN_setct_PCertReqData                                  = 'setct-PCertReqData';
 NID_setct_PCertReqData                                 = 556;
 SN_setct_PCertResTBS                                   = 'setct-PCertResTBS';
 NID_setct_PCertResTBS                                  = 557;
 SN_setct_BatchAdminReqData                             = 'setct-BatchAdminReqData';
 NID_setct_BatchAdminReqData                            = 558;
 SN_setct_BatchAdminResData                             = 'setct-BatchAdminResData';
 NID_setct_BatchAdminResData                            = 559;
 SN_setct_CardCInitResTBS                               = 'setct-CardCInitResTBS';
 NID_setct_CardCInitResTBS                              = 560;
 SN_setct_MeAqCInitResTBS                               = 'setct-MeAqCInitResTBS';
 NID_setct_MeAqCInitResTBS                              = 561;
 SN_setct_RegFormResTBS                                 = 'setct-RegFormResTBS';
 NID_setct_RegFormResTBS                                = 562;
 SN_setct_CertReqData                                   = 'setct-CertReqData';
 NID_setct_CertReqData                                  = 563;
 SN_setct_CertReqTBS                                    = 'setct-CertReqTBS';
 NID_setct_CertReqTBS                                   = 564;
 SN_setct_CertResData                                   = 'setct-CertResData';
 NID_setct_CertResData                                  = 565;
 SN_setct_CertInqReqTBS                                 = 'setct-CertInqReqTBS';
 NID_setct_CertInqReqTBS                                = 566;
 SN_setct_ErrorTBS                                      = 'setct-ErrorTBS';
 NID_setct_ErrorTBS                                     = 567;
 SN_setct_PIDualSignedTBE                               = 'setct-PIDualSignedTBE';
 NID_setct_PIDualSignedTBE                              = 568;
 SN_setct_PIUnsignedTBE                                 = 'setct-PIUnsignedTBE';
 NID_setct_PIUnsignedTBE                                = 569;
 SN_setct_AuthReqTBE                                    = 'setct-AuthReqTBE';
 NID_setct_AuthReqTBE                                   = 570;
 SN_setct_AuthResTBE                                    = 'setct-AuthResTBE';
 NID_setct_AuthResTBE                                   = 571;
 SN_setct_AuthResTBEX                                   = 'setct-AuthResTBEX';
 NID_setct_AuthResTBEX                                  = 572;
 SN_setct_AuthTokenTBE                                  = 'setct-AuthTokenTBE';
 NID_setct_AuthTokenTBE                                 = 573;
 SN_setct_CapTokenTBE                                   = 'setct-CapTokenTBE';
 NID_setct_CapTokenTBE                                  = 574;
 SN_setct_CapTokenTBEX                                  = 'setct-CapTokenTBEX';
 NID_setct_CapTokenTBEX                                 = 575;
 SN_setct_AcqCardCodeMsgTBE                             = 'setct-AcqCardCodeMsgTBE';
 NID_setct_AcqCardCodeMsgTBE                            = 576;
 SN_setct_AuthRevReqTBE                                 = 'setct-AuthRevReqTBE';
 NID_setct_AuthRevReqTBE                                = 577;
 SN_setct_AuthRevResTBE                                 = 'setct-AuthRevResTBE';
 NID_setct_AuthRevResTBE                                = 578;
 SN_setct_AuthRevResTBEB                                = 'setct-AuthRevResTBEB';
 NID_setct_AuthRevResTBEB                               = 579;
 SN_setct_CapReqTBE                                     = 'setct-CapReqTBE';
 NID_setct_CapReqTBE                                    = 580;
 SN_setct_CapReqTBEX                                    = 'setct-CapReqTBEX';
 NID_setct_CapReqTBEX                                   = 581;
 SN_setct_CapResTBE                                     = 'setct-CapResTBE';
 NID_setct_CapResTBE                                    = 582;
 SN_setct_CapRevReqTBE                                  = 'setct-CapRevReqTBE';
 NID_setct_CapRevReqTBE                                 = 583;
 SN_setct_CapRevReqTBEX                                 = 'setct-CapRevReqTBEX';
 NID_setct_CapRevReqTBEX                                = 584;
 SN_setct_CapRevResTBE                                  = 'setct-CapRevResTBE';
 NID_setct_CapRevResTBE                                 = 585;
 SN_setct_CredReqTBE                                    = 'setct-CredReqTBE';
 NID_setct_CredReqTBE                                   = 586;
 SN_setct_CredReqTBEX                                   = 'setct-CredReqTBEX';
 NID_setct_CredReqTBEX                                  = 587;
 SN_setct_CredResTBE                                    = 'setct-CredResTBE';
 NID_setct_CredResTBE                                   = 588;
 SN_setct_CredRevReqTBE                                 = 'setct-CredRevReqTBE';
 NID_setct_CredRevReqTBE                                = 589;
 SN_setct_CredRevReqTBEX                                = 'setct-CredRevReqTBEX';
 NID_setct_CredRevReqTBEX                               = 590;
 SN_setct_CredRevResTBE                                 = 'setct-CredRevResTBE';
 NID_setct_CredRevResTBE                                = 591;
 SN_setct_BatchAdminReqTBE                              = 'setct-BatchAdminReqTBE';
 NID_setct_BatchAdminReqTBE                             = 592;
 SN_setct_BatchAdminResTBE                              = 'setct-BatchAdminResTBE';
 NID_setct_BatchAdminResTBE                             = 593;
 SN_setct_RegFormReqTBE                                 = 'setct-RegFormReqTBE';
 NID_setct_RegFormReqTBE                                = 594;
 SN_setct_CertReqTBE                                    = 'setct-CertReqTBE';
 NID_setct_CertReqTBE                                   = 595;
 SN_setct_CertReqTBEX                                   = 'setct-CertReqTBEX';
 NID_setct_CertReqTBEX                                  = 596;
 SN_setct_CertResTBE                                    = 'setct-CertResTBE';
 NID_setct_CertResTBE                                   = 597;
 SN_setct_CRLNotificationTBS                            = 'setct-CRLNotificationTBS';
 NID_setct_CRLNotificationTBS                           = 598;
 SN_setct_CRLNotificationResTBS                         = 'setct-CRLNotificationResTBS';
 NID_setct_CRLNotificationResTBS                        = 599;
 SN_setct_BCIDistributionTBS                            = 'setct-BCIDistributionTBS';
 NID_setct_BCIDistributionTBS                           = 600;
 SN_setext_genCrypt                                     = 'setext-genCrypt';
 LN_setext_genCrypt                                     = 'generic cryptogram';
 NID_setext_genCrypt                                    = 601;
 SN_setext_miAuth                                       = 'setext-miAuth';
 LN_setext_miAuth                                       = 'merchant initiated auth';
 NID_setext_miAuth                                      = 602;
 SN_setext_pinSecure                                    = 'setext-pinSecure';
 NID_setext_pinSecure                                   = 603;
 SN_setext_pinAny                                       = 'setext-pinAny';
 NID_setext_pinAny                                      = 604;
 SN_setext_track2                                       = 'setext-track2';
 NID_setext_track2                                      = 605;
 SN_setext_cv                                           = 'setext-cv';
 LN_setext_cv                                           = 'additional verification';
 NID_setext_cv                                          = 606;
 SN_set_policy_root                                     = 'set-policy-root';
 NID_set_policy_root                                    = 607;
 SN_setCext_hashedRoot                                  = 'setCext-hashedRoot';
 NID_setCext_hashedRoot                                 = 608;
 SN_setCext_certType                                    = 'setCext-certType';
 NID_setCext_certType                                   = 609;
 SN_setCext_merchData                                   = 'setCext-merchData';
 NID_setCext_merchData                                  = 610;
 SN_setCext_cCertRequired                               = 'setCext-cCertRequired';
 NID_setCext_cCertRequired                              = 611;
 SN_setCext_tunneling                                   = 'setCext-tunneling';
 NID_setCext_tunneling                                  = 612;
 SN_setCext_setExt                                      = 'setCext-setExt';
 NID_setCext_setExt                                     = 613;
 SN_setCext_setQualf                                    = 'setCext-setQualf';
 NID_setCext_setQualf                                   = 614;
 SN_setCext_PGWYcapabilities                            = 'setCext-PGWYcapabilities';
 NID_setCext_PGWYcapabilities                           = 615;
 SN_setCext_TokenIdentifier                             = 'setCext-TokenIdentifier';
 NID_setCext_TokenIdentifier                            = 616;
 SN_setCext_Track2Data                                  = 'setCext-Track2Data';
 NID_setCext_Track2Data                                 = 617;
 SN_setCext_TokenType                                   = 'setCext-TokenType';
 NID_setCext_TokenType                                  = 618;
 SN_setCext_IssuerCapabilities                          = 'setCext-IssuerCapabilities';
 NID_setCext_IssuerCapabilities                         = 619;
 SN_setAttr_Cert                                        = 'setAttr-Cert';
 NID_setAttr_Cert                                       = 620;
 SN_setAttr_PGWYcap                                     = 'setAttr-PGWYcap';
 LN_setAttr_PGWYcap                                     = 'payment gateway capabilities';
 NID_setAttr_PGWYcap                                    = 621;
 SN_setAttr_TokenType                                   = 'setAttr-TokenType';
 NID_setAttr_TokenType                                  = 622;
 SN_setAttr_IssCap                                      = 'setAttr-IssCap';
 LN_setAttr_IssCap                                      = 'issuer capabilities';
 NID_setAttr_IssCap                                     = 623;
 SN_set_rootKeyThumb                                    = 'set-rootKeyThumb';
 NID_set_rootKeyThumb                                   = 624;
 SN_set_addPolicy                                       = 'set-addPolicy';
 NID_set_addPolicy                                      = 625;
 SN_setAttr_Token_EMV                                   = 'setAttr-Token-EMV';
 NID_setAttr_Token_EMV                                  = 626;
 SN_setAttr_Token_B0Prime                               = 'setAttr-Token-B0Prime';
 NID_setAttr_Token_B0Prime                              = 627;
 SN_setAttr_IssCap_CVM                                  = 'setAttr-IssCap-CVM';
 NID_setAttr_IssCap_CVM                                 = 628;
 SN_setAttr_IssCap_T2                                   = 'setAttr-IssCap-T2';
 NID_setAttr_IssCap_T2                                  = 629;
 SN_setAttr_IssCap_Sig                                  = 'setAttr-IssCap-Sig';
 NID_setAttr_IssCap_Sig                                 = 630;
 SN_setAttr_GenCryptgrm                                 = 'setAttr-GenCryptgrm';
 LN_setAttr_GenCryptgrm                                 = 'generate cryptogram';
 NID_setAttr_GenCryptgrm                                = 631;
 SN_setAttr_T2Enc                                       = 'setAttr-T2Enc';
 LN_setAttr_T2Enc                                       = 'encrypted track 2';
 NID_setAttr_T2Enc                                      = 632;
 SN_setAttr_T2cleartxt                                  = 'setAttr-T2cleartxt';
 LN_setAttr_T2cleartxt                                  = 'cleartext track 2';
 NID_setAttr_T2cleartxt                                 = 633;
 SN_setAttr_TokICCsig                                   = 'setAttr-TokICCsig';
 LN_setAttr_TokICCsig                                   = 'ICC or token signature';
 NID_setAttr_TokICCsig                                  = 634;
 SN_setAttr_SecDevSig                                   = 'setAttr-SecDevSig';
 LN_setAttr_SecDevSig                                   = 'secure device signature';
 NID_setAttr_SecDevSig                                  = 635;
 SN_set_brand_IATA_ATA                                  = 'set-brand-IATA-ATA';
 NID_set_brand_IATA_ATA                                 = 636;
 SN_set_brand_Diners                                    = 'set-brand-Diners';
 NID_set_brand_Diners                                   = 637;
 SN_set_brand_AmericanExpress                           = 'set-brand-AmericanExpress';
 NID_set_brand_AmericanExpress                          = 638;
 SN_set_brand_JCB                                       = 'set-brand-JCB';
 NID_set_brand_JCB                                      = 639;
 SN_set_brand_Visa                                      = 'set-brand-Visa';
 NID_set_brand_Visa                                     = 640;
 SN_set_brand_MasterCard                                = 'set-brand-MasterCard';
 NID_set_brand_MasterCard                               = 641;
 SN_set_brand_Novus                                     = 'set-brand-Novus';
 NID_set_brand_Novus                                    = 642;
 SN_des_cdmf                                            = 'DES-CDMF';
 LN_des_cdmf                                            = 'des-cdmf';
 NID_des_cdmf                                           = 643;
 SN_rsaOAEPEncryptionSET                                = 'rsaOAEPEncryptionSET';
 NID_rsaOAEPEncryptionSET                               = 644;
 SN_ipsec3                                              = 'Oakley-EC2N-3';
 LN_ipsec3                                              = 'ipsec3';
 NID_ipsec3                                             = 749;
 SN_ipsec4                                              = 'Oakley-EC2N-4';
 LN_ipsec4                                              = 'ipsec4';
 NID_ipsec4                                             = 750;
 SN_whirlpool                                           = 'whirlpool';
 NID_whirlpool                                          = 804;
 SN_cryptopro                                           = 'cryptopro';
 NID_cryptopro                                          = 805;
 SN_cryptocom                                           = 'cryptocom';
 NID_cryptocom                                          = 806;
 SN_id_GostR3411_94_with_GostR3410_2001                 = 'id-GostR3411-94-with-GostR3410-2001';
 LN_id_GostR3411_94_with_GostR3410_2001                 = 'GOST R 34.11-94 with GOST R 34.10-2001';
 NID_id_GostR3411_94_with_GostR3410_2001                = 807;
 SN_id_GostR3411_94_with_GostR3410_94                   = 'id-GostR3411-94-with-GostR3410-94';
 LN_id_GostR3411_94_with_GostR3410_94                   = 'GOST R 34.11-94 with GOST R 34.10-94';
 NID_id_GostR3411_94_with_GostR3410_94                  = 808;
 SN_id_GostR3411_94                                     = 'md_gost94';
 LN_id_GostR3411_94                                     = 'GOST R 34.11-94';
 NID_id_GostR3411_94                                    = 809;
 SN_id_HMACGostR3411_94                                 = 'id-HMACGostR3411-94';
 LN_id_HMACGostR3411_94                                 = 'HMAC GOST 34.11-94';
 NID_id_HMACGostR3411_94                                = 810;
 SN_id_GostR3410_2001                                   = 'gost2001';
 LN_id_GostR3410_2001                                   = 'GOST R 34.10-2001';
 NID_id_GostR3410_2001                                  = 811;
 SN_id_GostR3410_94                                     = 'gost94';
 LN_id_GostR3410_94                                     = 'GOST R 34.10-94';
 NID_id_GostR3410_94                                    = 812;
 SN_id_Gost28147_89                                     = 'gost89';
 LN_id_Gost28147_89                                     = 'GOST 28147-89';
 NID_id_Gost28147_89                                    = 813;
 SN_gost89_cnt                                          = 'gost89-cnt';
 NID_gost89_cnt                                         = 814;
 SN_id_Gost28147_89_MAC                                 = 'gost-mac';
 LN_id_Gost28147_89_MAC                                 = 'GOST 28147-89 MAC';
 NID_id_Gost28147_89_MAC                                = 815;
 SN_id_GostR3411_94_prf                                 = 'prf-gostr3411-94';
 LN_id_GostR3411_94_prf                                 = 'GOST R 34.11-94 PRF';
 NID_id_GostR3411_94_prf                                = 816;
 SN_id_GostR3410_2001DH                                 = 'id-GostR3410-2001DH';
 LN_id_GostR3410_2001DH                                 = 'GOST R 34.10-2001 DH';
 NID_id_GostR3410_2001DH                                = 817;
 SN_id_GostR3410_94DH                                   = 'id-GostR3410-94DH';
 LN_id_GostR3410_94DH                                   = 'GOST R 34.10-94 DH';
 NID_id_GostR3410_94DH                                  = 818;
 SN_id_Gost28147_89_CryptoPro_KeyMeshing                = 'id-Gost28147-89-CryptoPro-KeyMeshing';
 NID_id_Gost28147_89_CryptoPro_KeyMeshing               = 819;
 SN_id_Gost28147_89_None_KeyMeshing                     = 'id-Gost28147-89-None-KeyMeshing';
 NID_id_Gost28147_89_None_KeyMeshing                    = 820;
 SN_id_GostR3411_94_TestParamSet                        = 'id-GostR3411-94-TestParamSet';
 NID_id_GostR3411_94_TestParamSet                       = 821;
 SN_id_GostR3411_94_CryptoProParamSet                   = 'id-GostR3411-94-CryptoProParamSet';
 NID_id_GostR3411_94_CryptoProParamSet                  = 822;
 SN_id_Gost28147_89_TestParamSet                        = 'id-Gost28147-89-TestParamSet';
 NID_id_Gost28147_89_TestParamSet                       = 823;
 SN_id_Gost28147_89_CryptoPro_A_ParamSet                = 'id-Gost28147-89-CryptoPro-A-ParamSet';
 NID_id_Gost28147_89_CryptoPro_A_ParamSet               = 824;
 SN_id_Gost28147_89_CryptoPro_B_ParamSet                = 'id-Gost28147-89-CryptoPro-B-ParamSet';
 NID_id_Gost28147_89_CryptoPro_B_ParamSet               = 825;
 SN_id_Gost28147_89_CryptoPro_C_ParamSet                = 'id-Gost28147-89-CryptoPro-C-ParamSet';
 NID_id_Gost28147_89_CryptoPro_C_ParamSet               = 826;
 SN_id_Gost28147_89_CryptoPro_D_ParamSet                = 'id-Gost28147-89-CryptoPro-D-ParamSet';
 NID_id_Gost28147_89_CryptoPro_D_ParamSet               = 827;
 SN_id_Gost28147_89_CryptoPro_Oscar_1_1_ParamSet        = 'id-Gost28147-89-CryptoPro-Oscar-1-1-ParamSet';
 NID_id_Gost28147_89_CryptoPro_Oscar_1_1_ParamSet       = 828;
 SN_id_Gost28147_89_CryptoPro_Oscar_1_0_ParamSet        = 'id-Gost28147-89-CryptoPro-Oscar-1-0-ParamSet';
 NID_id_Gost28147_89_CryptoPro_Oscar_1_0_ParamSet       = 829;
 SN_id_Gost28147_89_CryptoPro_RIC_1_ParamSet            = 'id-Gost28147-89-CryptoPro-RIC-1-ParamSet';
 NID_id_Gost28147_89_CryptoPro_RIC_1_ParamSet           = 830;
 SN_id_GostR3410_94_TestParamSet                        = 'id-GostR3410-94-TestParamSet';
 NID_id_GostR3410_94_TestParamSet                       = 831;
 SN_id_GostR3410_94_CryptoPro_A_ParamSet                = 'id-GostR3410-94-CryptoPro-A-ParamSet';
 NID_id_GostR3410_94_CryptoPro_A_ParamSet               = 832;
 SN_id_GostR3410_94_CryptoPro_B_ParamSet                = 'id-GostR3410-94-CryptoPro-B-ParamSet';
 NID_id_GostR3410_94_CryptoPro_B_ParamSet               = 833;
 SN_id_GostR3410_94_CryptoPro_C_ParamSet                = 'id-GostR3410-94-CryptoPro-C-ParamSet';
 NID_id_GostR3410_94_CryptoPro_C_ParamSet               = 834;
 SN_id_GostR3410_94_CryptoPro_D_ParamSet                = 'id-GostR3410-94-CryptoPro-D-ParamSet';
 NID_id_GostR3410_94_CryptoPro_D_ParamSet               = 835;
 SN_id_GostR3410_94_CryptoPro_XchA_ParamSet             = 'id-GostR3410-94-CryptoPro-XchA-ParamSet';
 NID_id_GostR3410_94_CryptoPro_XchA_ParamSet            = 836;
 SN_id_GostR3410_94_CryptoPro_XchB_ParamSet             = 'id-GostR3410-94-CryptoPro-XchB-ParamSet';
 NID_id_GostR3410_94_CryptoPro_XchB_ParamSet            = 837;
 SN_id_GostR3410_94_CryptoPro_XchC_ParamSet             = 'id-GostR3410-94-CryptoPro-XchC-ParamSet';
 NID_id_GostR3410_94_CryptoPro_XchC_ParamSet            = 838;
 SN_id_GostR3410_2001_TestParamSet                      = 'id-GostR3410-2001-TestParamSet';
 NID_id_GostR3410_2001_TestParamSet                     = 839;
 SN_id_GostR3410_2001_CryptoPro_A_ParamSet              = 'id-GostR3410-2001-CryptoPro-A-ParamSet';
 NID_id_GostR3410_2001_CryptoPro_A_ParamSet             = 840;
 SN_id_GostR3410_2001_CryptoPro_B_ParamSet              = 'id-GostR3410-2001-CryptoPro-B-ParamSet';
 NID_id_GostR3410_2001_CryptoPro_B_ParamSet             = 841;
 SN_id_GostR3410_2001_CryptoPro_C_ParamSet              = 'id-GostR3410-2001-CryptoPro-C-ParamSet';
 NID_id_GostR3410_2001_CryptoPro_C_ParamSet             = 842;
 SN_id_GostR3410_2001_CryptoPro_XchA_ParamSet           = 'id-GostR3410-2001-CryptoPro-XchA-ParamSet';
 NID_id_GostR3410_2001_CryptoPro_XchA_ParamSet          = 843;
 SN_id_GostR3410_2001_CryptoPro_XchB_ParamSet           = 'id-GostR3410-2001-CryptoPro-XchB-ParamSet';
 NID_id_GostR3410_2001_CryptoPro_XchB_ParamSet          = 844;
 SN_id_GostR3410_94_a                                   = 'id-GostR3410-94-a';
 NID_id_GostR3410_94_a                                  = 845;
 SN_id_GostR3410_94_aBis                                = 'id-GostR3410-94-aBis';
 NID_id_GostR3410_94_aBis                               = 846;
 SN_id_GostR3410_94_b                                   = 'id-GostR3410-94-b';
 NID_id_GostR3410_94_b                                  = 847;
 SN_id_GostR3410_94_bBis                                = 'id-GostR3410-94-bBis';
 NID_id_GostR3410_94_bBis                               = 848;
 SN_id_Gost28147_89_cc                                  = 'id-Gost28147-89-cc';
 LN_id_Gost28147_89_cc                                  = 'GOST 28147-89 Cryptocom ParamSet';
 NID_id_Gost28147_89_cc                                 = 849;
 SN_id_GostR3410_94_cc                                  = 'gost94cc';
 LN_id_GostR3410_94_cc                                  = 'GOST 34.10-94 Cryptocom';
 NID_id_GostR3410_94_cc                                 = 850;
 SN_id_GostR3410_2001_cc                                = 'gost2001cc';
 LN_id_GostR3410_2001_cc                                = 'GOST 34.10-2001 Cryptocom';
 NID_id_GostR3410_2001_cc                               = 851;
 SN_id_GostR3411_94_with_GostR3410_94_cc                = 'id-GostR3411-94-with-GostR3410-94-cc';
 LN_id_GostR3411_94_with_GostR3410_94_cc                = 'GOST R 34.11-94 with GOST R 34.10-94 Cryptocom';
 NID_id_GostR3411_94_with_GostR3410_94_cc               = 852;
 SN_id_GostR3411_94_with_GostR3410_2001_cc              = 'id-GostR3411-94-with-GostR3410-2001-cc';
 LN_id_GostR3411_94_with_GostR3410_2001_cc              = 'GOST R 34.11-94 with GOST R 34.10-2001 Cryptocom';
 NID_id_GostR3411_94_with_GostR3410_2001_cc             = 853;
 SN_id_GostR3410_2001_ParamSet_cc                       = 'id-GostR3410-2001-ParamSet-cc';
 LN_id_GostR3410_2001_ParamSet_cc                       = 'GOST R 3410-2001 Parameter Set Cryptocom';
 NID_id_GostR3410_2001_ParamSet_cc                      = 854;
 SN_camellia_128_cbc                                    = 'CAMELLIA-128-CBC';
 LN_camellia_128_cbc                                    = 'camellia-128-cbc';
 NID_camellia_128_cbc                                   = 751;
 SN_camellia_192_cbc                                    = 'CAMELLIA-192-CBC';
 LN_camellia_192_cbc                                    = 'camellia-192-cbc';
 NID_camellia_192_cbc                                   = 752;
 SN_camellia_256_cbc                                    = 'CAMELLIA-256-CBC';
 LN_camellia_256_cbc                                    = 'camellia-256-cbc';
 NID_camellia_256_cbc                                   = 753;
 SN_id_camellia128_wrap                                 = 'id-camellia128-wrap';
 NID_id_camellia128_wrap                                = 907;
 SN_id_camellia192_wrap                                 = 'id-camellia192-wrap';
 NID_id_camellia192_wrap                                = 908;
 SN_id_camellia256_wrap                                 = 'id-camellia256-wrap';
 NID_id_camellia256_wrap                                = 909;
 SN_camellia_128_ecb                                    = 'CAMELLIA-128-ECB';
 LN_camellia_128_ecb                                    = 'camellia-128-ecb';
 NID_camellia_128_ecb                                   = 754;
 SN_camellia_128_ofb128                                 = 'CAMELLIA-128-OFB';
 LN_camellia_128_ofb128                                 = 'camellia-128-ofb';
 NID_camellia_128_ofb128                                = 766;
 SN_camellia_128_cfb128                                 = 'CAMELLIA-128-CFB';
 LN_camellia_128_cfb128                                 = 'camellia-128-cfb';
 NID_camellia_128_cfb128                                = 757;
 SN_camellia_192_ecb                                    = 'CAMELLIA-192-ECB';
 LN_camellia_192_ecb                                    = 'camellia-192-ecb';
 NID_camellia_192_ecb                                   = 755;
 SN_camellia_192_ofb128                                 = 'CAMELLIA-192-OFB';
 LN_camellia_192_ofb128                                 = 'camellia-192-ofb';
 NID_camellia_192_ofb128                                = 767;
 SN_camellia_192_cfb128                                 = 'CAMELLIA-192-CFB';
 LN_camellia_192_cfb128                                 = 'camellia-192-cfb';
 NID_camellia_192_cfb128                                = 758;
 SN_camellia_256_ecb                                    = 'CAMELLIA-256-ECB';
 LN_camellia_256_ecb                                    = 'camellia-256-ecb';
 NID_camellia_256_ecb                                   = 756;
 SN_camellia_256_ofb128                                 = 'CAMELLIA-256-OFB';
 LN_camellia_256_ofb128                                 = 'camellia-256-ofb';
 NID_camellia_256_ofb128                                = 768;
 SN_camellia_256_cfb128                                 = 'CAMELLIA-256-CFB';
 LN_camellia_256_cfb128                                 = 'camellia-256-cfb';
 NID_camellia_256_cfb128                                = 759;
 SN_camellia_128_cfb1                                   = 'CAMELLIA-128-CFB1';
 LN_camellia_128_cfb1                                   = 'camellia-128-cfb1';
 NID_camellia_128_cfb1                                  = 760;
 SN_camellia_192_cfb1                                   = 'CAMELLIA-192-CFB1';
 LN_camellia_192_cfb1                                   = 'camellia-192-cfb1';
 NID_camellia_192_cfb1                                  = 761;
 SN_camellia_256_cfb1                                   = 'CAMELLIA-256-CFB1';
 LN_camellia_256_cfb1                                   = 'camellia-256-cfb1';
 NID_camellia_256_cfb1                                  = 762;
 SN_camellia_128_cfb8                                   = 'CAMELLIA-128-CFB8';
 LN_camellia_128_cfb8                                   = 'camellia-128-cfb8';
 NID_camellia_128_cfb8                                  = 763;
 SN_camellia_192_cfb8                                   = 'CAMELLIA-192-CFB8';
 LN_camellia_192_cfb8                                   = 'camellia-192-cfb8';
 NID_camellia_192_cfb8                                  = 764;
 SN_camellia_256_cfb8                                   = 'CAMELLIA-256-CFB8';
 LN_camellia_256_cfb8                                   = 'camellia-256-cfb8';
 NID_camellia_256_cfb8                                  = 765;
 SN_kisa                                                = 'KISA';
 LN_kisa                                                = 'kisa';
 NID_kisa                                               = 773;
 SN_seed_ecb                                            = 'SEED-ECB';
 LN_seed_ecb                                            = 'seed-ecb';
 NID_seed_ecb                                           = 776;
 SN_seed_cbc                                            = 'SEED-CBC';
 LN_seed_cbc                                            = 'seed-cbc';
 NID_seed_cbc                                           = 777;
 SN_seed_cfb128                                         = 'SEED-CFB';
 LN_seed_cfb128                                         = 'seed-cfb';
 NID_seed_cfb128                                        = 779;
 SN_seed_ofb128                                         = 'SEED-OFB';
 LN_seed_ofb128                                         = 'seed-ofb';
 NID_seed_ofb128                                        = 778;
 SN_hmac                                                = 'HMAC';
 LN_hmac                                                = 'hmac';
 NID_hmac                                               = 855;
 SN_cmac                                                = 'CMAC';
 LN_cmac                                                = 'cmac';
 NID_cmac                                               = 894;
 SN_rc4_hmac_md5                                        = 'RC4-HMAC-MD5';
 LN_rc4_hmac_md5                                        = 'rc4-hmac-md5';
 NID_rc4_hmac_md5                                       = 915;
 SN_aes_128_cbc_hmac_sha1                               = 'AES-128-CBC-HMAC-SHA1';
 LN_aes_128_cbc_hmac_sha1                               = 'aes-128-cbc-hmac-sha1';
 NID_aes_128_cbc_hmac_sha1                              = 916;
 SN_aes_192_cbc_hmac_sha1                               = 'AES-192-CBC-HMAC-SHA1';
 LN_aes_192_cbc_hmac_sha1                               = 'aes-192-cbc-hmac-sha1';
 NID_aes_192_cbc_hmac_sha1                              = 917;
 SN_aes_256_cbc_hmac_sha1                               = 'AES-256-CBC-HMAC-SHA1';
 LN_aes_256_cbc_hmac_sha1                               = 'aes-256-cbc-hmac-sha1';
 NID_aes_256_cbc_hmac_sha1                              = 918;




 OBJ_NAME_TYPE_UNDEF            = $00;
 OBJ_NAME_TYPE_MD_METH          = $01;
 OBJ_NAME_TYPE_CIPHER_METH      = $02;
 OBJ_NAME_TYPE_PKEY_METH        = $03;
 OBJ_NAME_TYPE_COMP_METH        = $04;
 OBJ_NAME_TYPE_NUM              = $05;
 OBJ_NAME_ALIAS                         = $8000;
 OBJ_BSEARCH_VALUE_ON_NOMATCH           = $01;
 OBJ_BSEARCH_FIRST_VALUE_ON_MATCH       = $02;


{$ENDREGION}

const
  ASIdentifierChoice_inherit = 0;
  ASIdentifierChoice_asIdsOrRanges = 1;
  IPAddressOrRange_addressPrefix = 0;
  IPAddressOrRange_addressRange = 1;
  IPAddressChoice_inherit = 0;
  IPAddressChoice_addressesOrRanges = 1;
  SHA_LBLOCK = 16;
  SHA_CBLOCK = 64;
  SHA_DIGEST_LENGTH = 20;
  SHA_LAST_BLOCK = (SHA_CBLOCK - 8);
  SHA256_CBLOCK = (SHA_LBLOCK * 4);
  SHA224_DIGEST_LENGTH = 28;
  SHA256_DIGEST_LENGTH = 32;
  SHA384_DIGEST_LENGTH = 48;
  SHA512_DIGEST_LENGTH = 64;
  SHA512_CBLOCK = (SHA_LBLOCK * 8);
  EVP_MAX_MD_SIZE = 64;
  EVP_MAX_KEY_LENGTH = 32;
  EVP_MAX_IV_LENGTH = 16;
  EVP_MAX_BLOCK_LENGTH = 32;
{$REGION 'AES'}
const
  AES_ENCRYPT    = 1;
  AES_DECRYPT    = 0;
  AES_MAXNR = 14;
  AES_BLOCK_SIZE = 16;
{$ENDREGION}
{$REGION 'BlowFish'}
const
    BF_ENCRYPT  = 1;
    BF_DECRYPT  = 0;
    BF_LONG_LOG2 = 3;
    BF_ROUNDS    = 16;
    BF_BLOCK    = 8;
{$ENDREGION}
{$REGION 'ASN'}
const
 V_ASN1_UNIVERSAL               = $00;
 V_ASN1_APPLICATION             = $40;
 V_ASN1_CONTEXT_SPECIFIC        = $80;
 V_ASN1_PRIVATE                 = $c0;
 V_ASN1_CONSTRUCTED         = $20;
 V_ASN1_PRIMITIVE_TAG       = $1f;
 V_ASN1_PRIMATIVE_TAG       = $1f;
 V_ASN1_APP_CHOOSE      = -2;
 V_ASN1_OTHER           = -3;
 V_ASN1_ANY             = -4;
 V_ASN1_NEG         = $100;
 V_ASN1_UNDEF                = -1;
 V_ASN1_EOC                  = 0;
 V_ASN1_BOOLEAN              = 1;
 V_ASN1_INTEGER              = 2;
 V_ASN1_NEG_INTEGER          = (2  or  V_ASN1_NEG);
 V_ASN1_BIT_STRING           = 3;
 V_ASN1_OCTET_STRING         = 4;
 V_ASN1_NULL                 = 5;
 V_ASN1_OBJECT               = 6;
 V_ASN1_OBJECT_DESCRIPTOR    = 7;
 V_ASN1_EXTERNAL             = 8;
 V_ASN1_REAL                 = 9;
 V_ASN1_ENUMERATED           = 10;
 V_ASN1_NEG_ENUMERATED       = (10  or  V_ASN1_NEG);
 V_ASN1_UTF8STRING           = 12;
 V_ASN1_SEQUENCE             = 16;
 V_ASN1_SET                  = 17;
 V_ASN1_NUMERICSTRING        = 18;
 V_ASN1_PRINTABLESTRING      = 19;
 V_ASN1_T61STRING            = 20;
 V_ASN1_TELETEXSTRING        = 20;
 V_ASN1_VIDEOTEXSTRING       = 21;
 V_ASN1_IA5STRING            = 22;
 V_ASN1_UTCTIME              = 23;
 V_ASN1_GENERALIZEDTIME      = 24;
 V_ASN1_GRAPHICSTRING        = 25;
 V_ASN1_ISO64STRING          = 26;
 V_ASN1_VISIBLESTRING        = 26;
 V_ASN1_GENERALSTRING        = 27;
 V_ASN1_UNIVERSALSTRING      = 28;
 V_ASN1_BMPSTRING            = 30;
 B_ASN1_NUMERICSTRING   = $0001;
 B_ASN1_PRINTABLESTRING = $0002;
 B_ASN1_T61STRING       = $0004;
 B_ASN1_TELETEXSTRING   = $0004;
 B_ASN1_VIDEOTEXSTRING  = $0008;
 B_ASN1_IA5STRING       = $0010;
 B_ASN1_GRAPHICSTRING   = $0020;
 B_ASN1_ISO64STRING     = $0040;
 B_ASN1_VISIBLESTRING   = $0040;
 B_ASN1_GENERALSTRING   = $0080;
 B_ASN1_UNIVERSALSTRING = $0100;
 B_ASN1_OCTET_STRING    = $0200;
 B_ASN1_BIT_STRING      = $0400;
 B_ASN1_BMPSTRING       = $0800;
 B_ASN1_UNKNOWN         = $1000;
 B_ASN1_UTF8STRING      = $2000;
 B_ASN1_UTCTIME         = $4000;
 B_ASN1_GENERALIZEDTIME = $8000;
 B_ASN1_SEQUENCE        = $10000;
 MBSTRING_FLAG      = $1000;
 MBSTRING_UTF8      = (MBSTRING_FLAG);
 MBSTRING_ASC       = (MBSTRING_FLAG or 1);
 MBSTRING_BMP       = (MBSTRING_FLAG or 2);
 MBSTRING_UNIV      = (MBSTRING_FLAG or 4);
 SMIME_OLDMIME      = $400;
 SMIME_CRLFEOL      = $800;
 SMIME_STREAM       = $1000;
 ASN1_OBJECT_FLAG_DYNAMIC         = $01;
 ASN1_OBJECT_FLAG_CRITICAL        = $02;
 ASN1_OBJECT_FLAG_DYNAMIC_STRINGS = $04;
 ASN1_OBJECT_FLAG_DYNAMIC_DATA    = $08;
 ASN1_STRING_FLAG_BITS_LEFT       = $08;
 ASN1_STRING_FLAG_NDEF            = $010;
 ASN1_STRING_FLAG_CONT            = $020;
 ASN1_STRING_FLAG_MSTRING         = $040 ;
 ASN1_LONG_UNDEF    = $7fffffff;
 STABLE_FLAGS_MALLOC    = $01;
 STABLE_NO_MASK         = $02;
 DIRSTRING_TYPE         = (B_ASN1_PRINTABLESTRING or B_ASN1_T61STRING or B_ASN1_BMPSTRING or B_ASN1_UTF8STRING);
 PKCS9STRING_TYPE       = (DIRSTRING_TYPE or B_ASN1_IA5STRING);

 ub_name                    = 32768;
 ub_common_name             = 64;
 ub_locality_name           = 128;
 ub_state_name              = 128;
 ub_organization_name       = 64;
 ub_organization_unit_name  = 64;
 ub_title                   = 64;
 ub_email_address           = 128;

 ASN1_STRFLGS_ESC_2253      = 1;
 ASN1_STRFLGS_ESC_CTRL      = 2;
 ASN1_STRFLGS_ESC_MSB       = 4;
 ASN1_STRFLGS_ESC_QUOTE     = 8;
 CHARTYPE_PRINTABLESTRING   = $10;
 CHARTYPE_FIRST_ESC_2253        = $20;
 CHARTYPE_LAST_ESC_2253     = $40;
 ASN1_STRFLGS_UTF8_CONVERT  = $10;
 ASN1_STRFLGS_IGNORE_TYPE   = $20;
 ASN1_STRFLGS_DUMP_ALL      = $80;
 ASN1_STRFLGS_DUMP_UNKNOWN  = $100;
 ASN1_STRFLGS_DUMP_DER      = $200;
 ASN1_STRFLGS_RFC2253   = (ASN1_STRFLGS_ESC_2253  or  ASN1_STRFLGS_ESC_CTRL  or  ASN1_STRFLGS_ESC_MSB  or  ASN1_STRFLGS_UTF8_CONVERT  or  ASN1_STRFLGS_DUMP_UNKNOWN  or  ASN1_STRFLGS_DUMP_DER);
 ASN1_PCTX_FLAGS_SHOW_ABSENT            = $001  ;
 ASN1_PCTX_FLAGS_SHOW_SEQUENCE          = $002;
 ASN1_PCTX_FLAGS_SHOW_SSOF              = $004;
 ASN1_PCTX_FLAGS_SHOW_TYPE              = $008;
 ASN1_PCTX_FLAGS_NO_ANY_TYPE            = $010;
 ASN1_PCTX_FLAGS_NO_MSTRING_TYPE        = $020;
 ASN1_PCTX_FLAGS_NO_FIELD_NAME          = $040;
 ASN1_PCTX_FLAGS_SHOW_FIELD_STRUCT_NAME = $080;
 ASN1_PCTX_FLAGS_NO_STRUCT_NAME         = $100;
 ASN1_F_A2D_ASN1_OBJECT                      = 100;
 ASN1_F_A2I_ASN1_ENUMERATED                  = 101;
 ASN1_F_A2I_ASN1_INTEGER                     = 102;
 ASN1_F_A2I_ASN1_STRING                      = 103;
 ASN1_F_APPEND_EXP                           = 176;
 ASN1_F_ASN1_BIT_STRING_SET_BIT              = 183;
 ASN1_F_ASN1_CB                              = 177;
 ASN1_F_ASN1_CHECK_TLEN                      = 104;
 ASN1_F_ASN1_COLLATE_PRIMITIVE               = 105;
 ASN1_F_ASN1_COLLECT                         = 106;
 ASN1_F_ASN1_D2I_EX_PRIMITIVE                = 108;
 ASN1_F_ASN1_D2I_FP                          = 109;
 ASN1_F_ASN1_D2I_READ_BIO                    = 107;
 ASN1_F_ASN1_DIGEST                          = 184;
 ASN1_F_ASN1_DO_ADB                          = 110;
 ASN1_F_ASN1_DUP                             = 111;
 ASN1_F_ASN1_ENUMERATED_SET                  = 112;
 ASN1_F_ASN1_ENUMERATED_TO_BN                = 113;
 ASN1_F_ASN1_EX_C2I                          = 204;
 ASN1_F_ASN1_FIND_END                        = 190;
 ASN1_F_ASN1_GENERALIZEDTIME_ADJ             = 216;
 ASN1_F_ASN1_GENERALIZEDTIME_SET             = 185;
 ASN1_F_ASN1_GENERATE_V3                     = 178;
 ASN1_F_ASN1_GET_OBJECT                      = 114;
 ASN1_F_ASN1_HEADER_NEW                      = 115;
 ASN1_F_ASN1_I2D_BIO                         = 116;
 ASN1_F_ASN1_I2D_FP                          = 117;
 ASN1_F_ASN1_INTEGER_SET                     = 118;
 ASN1_F_ASN1_INTEGER_TO_BN                   = 119;
 ASN1_F_ASN1_ITEM_D2I_FP                     = 206;
 ASN1_F_ASN1_ITEM_DUP                        = 191;
 ASN1_F_ASN1_ITEM_EX_COMBINE_NEW             = 121;
 ASN1_F_ASN1_ITEM_EX_D2I                     = 120;
 ASN1_F_ASN1_ITEM_I2D_BIO                    = 192;
 ASN1_F_ASN1_ITEM_I2D_FP                     = 193;
 ASN1_F_ASN1_ITEM_PACK                       = 198;
 ASN1_F_ASN1_ITEM_SIGN                       = 195;
 ASN1_F_ASN1_ITEM_SIGN_CTX                   = 220;
 ASN1_F_ASN1_ITEM_UNPACK                     = 199;
 ASN1_F_ASN1_ITEM_VERIFY                     = 197;
 ASN1_F_ASN1_MBSTRING_NCOPY                  = 122;
 ASN1_F_ASN1_OBJECT_NEW                      = 123;
 ASN1_F_ASN1_OUTPUT_DATA                     = 214;
 ASN1_F_ASN1_PACK_STRING                     = 124;
 ASN1_F_ASN1_PCTX_NEW                        = 205;
 ASN1_F_ASN1_PKCS5_PBE_SET                   = 125;
 ASN1_F_ASN1_SEQ_PACK                        = 126;
 ASN1_F_ASN1_SEQ_UNPACK                      = 127;
 ASN1_F_ASN1_SIGN                            = 128;
 ASN1_F_ASN1_STR2TYPE                        = 179;
 ASN1_F_ASN1_STRING_SET                      = 186;
 ASN1_F_ASN1_STRING_TABLE_ADD                = 129;
 ASN1_F_ASN1_STRING_TYPE_NEW                 = 130;
 ASN1_F_ASN1_TEMPLATE_EX_D2I                 = 132;
 ASN1_F_ASN1_TEMPLATE_NEW                    = 133;
 ASN1_F_ASN1_TEMPLATE_NOEXP_D2I              = 131;
 ASN1_F_ASN1_TIME_ADJ                        = 217;
 ASN1_F_ASN1_TIME_SET                        = 175;
 ASN1_F_ASN1_TYPE_GET_INT_OCTETSTRING        = 134;
 ASN1_F_ASN1_TYPE_GET_OCTETSTRING            = 135;
 ASN1_F_ASN1_UNPACK_STRING                   = 136;
 ASN1_F_ASN1_UTCTIME_ADJ                     = 218;
 ASN1_F_ASN1_UTCTIME_SET                     = 187;
 ASN1_F_ASN1_VERIFY                          = 137;
 ASN1_F_B64_READ_ASN1                        = 209;
 ASN1_F_B64_WRITE_ASN1                       = 210;
 ASN1_F_BIO_NEW_NDEF                         = 208;
 ASN1_F_BITSTR_CB                            = 180;
 ASN1_F_BN_TO_ASN1_ENUMERATED                = 138;
 ASN1_F_BN_TO_ASN1_INTEGER                   = 139;
 ASN1_F_C2I_ASN1_BIT_STRING                  = 189;
 ASN1_F_C2I_ASN1_INTEGER                     = 194;
 ASN1_F_C2I_ASN1_OBJECT                      = 196;
 ASN1_F_COLLECT_DATA                         = 140;
 ASN1_F_D2I_ASN1_BIT_STRING                  = 141;
 ASN1_F_D2I_ASN1_BOOLEAN                     = 142;
 ASN1_F_D2I_ASN1_BYTES                       = 143;
 ASN1_F_D2I_ASN1_GENERALIZEDTIME             = 144;
 ASN1_F_D2I_ASN1_HEADER                      = 145;
 ASN1_F_D2I_ASN1_INTEGER                     = 146;
 ASN1_F_D2I_ASN1_OBJECT                      = 147;
 ASN1_F_D2I_ASN1_SET                         = 148;
 ASN1_F_D2I_ASN1_TYPE_BYTES                  = 149;
 ASN1_F_D2I_ASN1_UINTEGER                    = 150;
 ASN1_F_D2I_ASN1_UTCTIME                     = 151;
 ASN1_F_D2I_AUTOPRIVATEKEY                   = 207;
 ASN1_F_D2I_NETSCAPE_RSA                     = 152;
 ASN1_F_D2I_NETSCAPE_RSA_2                   = 153;
 ASN1_F_D2I_PRIVATEKEY                       = 154;
 ASN1_F_D2I_PUBLICKEY                        = 155;
 ASN1_F_D2I_RSA_NET                          = 200;
 ASN1_F_D2I_RSA_NET_2                        = 201;
 ASN1_F_D2I_X509                             = 156;
 ASN1_F_D2I_X509_CINF                        = 157;
 ASN1_F_D2I_X509_PKEY                        = 159;
 ASN1_F_I2D_ASN1_BIO_STREAM                  = 211;
 ASN1_F_I2D_ASN1_SET                         = 188;
 ASN1_F_I2D_ASN1_TIME                        = 160;
 ASN1_F_I2D_DSA_PUBKEY                       = 161;
 ASN1_F_I2D_EC_PUBKEY                        = 181;
 ASN1_F_I2D_PRIVATEKEY                       = 163;
 ASN1_F_I2D_PUBLICKEY                        = 164;
 ASN1_F_I2D_RSA_NET                          = 162;
 ASN1_F_I2D_RSA_PUBKEY                       = 165;
 ASN1_F_LONG_C2I                             = 166;
 ASN1_F_OID_MODULE_INIT                      = 174;
 ASN1_F_PARSE_TAGGING                        = 182;
 ASN1_F_PKCS5_PBE2_SET_IV                    = 167;
 ASN1_F_PKCS5_PBE_SET                        = 202;
 ASN1_F_PKCS5_PBE_SET0_ALGOR                 = 215;
 ASN1_F_PKCS5_PBKDF2_SET                     = 219;
 ASN1_F_SMIME_READ_ASN1                      = 212;
 ASN1_F_SMIME_TEXT                           = 213;
 ASN1_F_X509_CINF_NEW                        = 168;
 ASN1_F_X509_CRL_ADD0_REVOKED                = 169;
 ASN1_F_X509_INFO_NEW                        = 170;
 ASN1_F_X509_NAME_ENCODE                     = 203;
 ASN1_F_X509_NAME_EX_D2I                     = 158;
 ASN1_F_X509_NAME_EX_NEW                     = 171;
 ASN1_F_X509_NEW                             = 172;
 ASN1_F_X509_PKEY_NEW                        = 173;
//* Reason codes. */
 ASN1_R_ADDING_OBJECT                                       = 171;
 ASN1_R_ASN1_PARSE_ERROR                                    = 203;
 ASN1_R_ASN1_SIG_PARSE_ERROR                                = 204;
 ASN1_R_AUX_ERROR                                           = 100;
 ASN1_R_BAD_CLASS                                           = 101;
 ASN1_R_BAD_OBJECT_HEADER                                   = 102;
 ASN1_R_BAD_PASSWORD_READ                                   = 103;
 ASN1_R_BAD_TAG                                             = 104;
 ASN1_R_BMPSTRING_IS_WRONG_LENGTH                           = 214;
 ASN1_R_BN_LIB                                              = 105;
 ASN1_R_BOOLEAN_IS_WRONG_LENGTH                             = 106;
 ASN1_R_BUFFER_TOO_SMALL                                    = 107;
 ASN1_R_CIPHER_HAS_NO_OBJECT_IDENTIFIER                     = 108;
 ASN1_R_CONTEXT_NOT_INITIALISED                             = 217;
 ASN1_R_DATA_IS_WRONG                                       = 109;
 ASN1_R_DECODE_ERROR                                        = 110;
 ASN1_R_DECODING_ERROR                                      = 111;
 ASN1_R_DEPTH_EXCEEDED                                      = 174;
 ASN1_R_DIGEST_AND_KEY_TYPE_NOT_SUPPORTED                   = 198;
 ASN1_R_ENCODE_ERROR                                        = 112;
 ASN1_R_ERROR_GETTING_TIME                                  = 173;
 ASN1_R_ERROR_LOADING_SECTION                               = 172;
 ASN1_R_ERROR_PARSING_SET_ELEMENT                           = 113;
 ASN1_R_ERROR_SETTING_CIPHER_PARAMS                         = 114;
 ASN1_R_EXPECTING_AN_INTEGER                                = 115;
 ASN1_R_EXPECTING_AN_OBJECT                                 = 116;
 ASN1_R_EXPECTING_A_BOOLEAN                                 = 117;
 ASN1_R_EXPECTING_A_TIME                                    = 118;
 ASN1_R_EXPLICIT_LENGTH_MISMATCH                            = 119;
 ASN1_R_EXPLICIT_TAG_NOT_CONSTRUCTED                        = 120;
 ASN1_R_FIELD_MISSING                                       = 121;
 ASN1_R_FIRST_NUM_TOO_LARGE                                 = 122;
 ASN1_R_HEADER_TOO_LONG                                     = 123;
 ASN1_R_ILLEGAL_BITSTRING_FORMAT                            = 175;
 ASN1_R_ILLEGAL_BOOLEAN                                     = 176;
 ASN1_R_ILLEGAL_CHARACTERS                                  = 124;
 ASN1_R_ILLEGAL_FORMAT                                      = 177;
 ASN1_R_ILLEGAL_HEX                                         = 178;
 ASN1_R_ILLEGAL_IMPLICIT_TAG                                = 179;
 ASN1_R_ILLEGAL_INTEGER                                     = 180;
 ASN1_R_ILLEGAL_NESTED_TAGGING                              = 181;
 ASN1_R_ILLEGAL_NULL                                        = 125;
 ASN1_R_ILLEGAL_NULL_VALUE                                  = 182;
 ASN1_R_ILLEGAL_OBJECT                                      = 183;
 ASN1_R_ILLEGAL_OPTIONAL_ANY                                = 126;
 ASN1_R_ILLEGAL_OPTIONS_ON_ITEM_TEMPLATE                    = 170;
 ASN1_R_ILLEGAL_TAGGED_ANY                                  = 127;
 ASN1_R_ILLEGAL_TIME_VALUE                                  = 184;
 ASN1_R_INTEGER_NOT_ASCII_FORMAT                            = 185;
 ASN1_R_INTEGER_TOO_LARGE_FOR_LONG                          = 128;
 ASN1_R_INVALID_BMPSTRING_LENGTH                            = 129;
 ASN1_R_INVALID_DIGIT                                       = 130;
 ASN1_R_INVALID_MIME_TYPE                                   = 205;
 ASN1_R_INVALID_MODIFIER                                    = 186;
 ASN1_R_INVALID_NUMBER                                      = 187;
 ASN1_R_INVALID_OBJECT_ENCODING                             = 216;
 ASN1_R_INVALID_SEPARATOR                                   = 131;
 ASN1_R_INVALID_TIME_FORMAT                                 = 132;
 ASN1_R_INVALID_UNIVERSALSTRING_LENGTH                      = 133;
 ASN1_R_INVALID_UTF8STRING                                  = 134;
 ASN1_R_IV_TOO_LARGE                                        = 135;
 ASN1_R_LENGTH_ERROR                                        = 136;
 ASN1_R_LIST_ERROR                                          = 188;
 ASN1_R_MIME_NO_CONTENT_TYPE                                = 206;
 ASN1_R_MIME_PARSE_ERROR                                    = 207;
 ASN1_R_MIME_SIG_PARSE_ERROR                                = 208;
 ASN1_R_MISSING_EOC                                         = 137;
 ASN1_R_MISSING_SECOND_NUMBER                               = 138;
 ASN1_R_MISSING_VALUE                                       = 189;
 ASN1_R_MSTRING_NOT_UNIVERSAL                               = 139;
 ASN1_R_MSTRING_WRONG_TAG                                   = 140;
 ASN1_R_NESTED_ASN1_STRING                                  = 197;
 ASN1_R_NON_HEX_CHARACTERS                                  = 141;
 ASN1_R_NOT_ASCII_FORMAT                                    = 190;
 ASN1_R_NOT_ENOUGH_DATA                                     = 142;
 ASN1_R_NO_CONTENT_TYPE                                     = 209;
 ASN1_R_NO_DEFAULT_DIGEST                                   = 201;
 ASN1_R_NO_MATCHING_CHOICE_TYPE                             = 143;
 ASN1_R_NO_MULTIPART_BODY_FAILURE                           = 210;
 ASN1_R_NO_MULTIPART_BOUNDARY                               = 211;
 ASN1_R_NO_SIG_CONTENT_TYPE                                 = 212;
 ASN1_R_NULL_IS_WRONG_LENGTH                                = 144;
 ASN1_R_OBJECT_NOT_ASCII_FORMAT                             = 191;
 ASN1_R_ODD_NUMBER_OF_CHARS                                 = 145;
 ASN1_R_PRIVATE_KEY_HEADER_MISSING                          = 146;
 ASN1_R_SECOND_NUMBER_TOO_LARGE                             = 147;
 ASN1_R_SEQUENCE_LENGTH_MISMATCH                            = 148;
 ASN1_R_SEQUENCE_NOT_CONSTRUCTED                            = 149;
 ASN1_R_SEQUENCE_OR_SET_NEEDS_CONFIG                        = 192;
 ASN1_R_SHORT_LINE                                          = 150;
 ASN1_R_SIG_INVALID_MIME_TYPE                               = 213;
 ASN1_R_STREAMING_NOT_SUPPORTED                             = 202;
 ASN1_R_STRING_TOO_LONG                                     = 151;
 ASN1_R_STRING_TOO_SHORT                                    = 152;
 ASN1_R_TAG_VALUE_TOO_HIGH                                  = 153;
 ASN1_R_THE_ASN1_OBJECT_IDENTIFIER_IS_NOT_KNOWN_FOR_THIS_MD = 154;
 ASN1_R_TIME_NOT_ASCII_FORMAT                               = 193;
 ASN1_R_TOO_LONG                                            = 155;
 ASN1_R_TYPE_NOT_CONSTRUCTED                                = 156;
 ASN1_R_UNABLE_TO_DECODE_RSA_KEY                            = 157;
 ASN1_R_UNABLE_TO_DECODE_RSA_PRIVATE_KEY                    = 158;
 ASN1_R_UNEXPECTED_EOC                                      = 159;
 ASN1_R_UNIVERSALSTRING_IS_WRONG_LENGTH                     = 215;
 ASN1_R_UNKNOWN_FORMAT                                      = 160;
 ASN1_R_UNKNOWN_MESSAGE_DIGEST_ALGORITHM                    = 161;
 ASN1_R_UNKNOWN_OBJECT_TYPE                                 = 162;
 ASN1_R_UNKNOWN_PUBLIC_KEY_TYPE                             = 163;
 ASN1_R_UNKNOWN_SIGNATURE_ALGORITHM                         = 199;
 ASN1_R_UNKNOWN_TAG                                         = 194;
 ASN1_R_UNKOWN_FORMAT                                       = 195;
 ASN1_R_UNSUPPORTED_ANY_DEFINED_BY_TYPE                     = 164;
 ASN1_R_UNSUPPORTED_CIPHER                                  = 165;
 ASN1_R_UNSUPPORTED_ENCRYPTION_ALGORITHM                    = 166;
 ASN1_R_UNSUPPORTED_PUBLIC_KEY_TYPE                         = 167;
 ASN1_R_UNSUPPORTED_TYPE                                    = 196;
 ASN1_R_WRONG_PUBLIC_KEY_TYPE                               = 200;
 ASN1_R_WRONG_TAG                                           = 168;
 ASN1_R_WRONG_TYPE                                          = 169;


 ASN1_ITYPE_PRIMITIVE       = $0;
 ASN1_ITYPE_SEQUENCE        = $1;
 ASN1_ITYPE_CHOICE          = $2;
 ASN1_ITYPE_COMPAT          = $3;
 ASN1_ITYPE_EXTERN          = $4;
 ASN1_ITYPE_MSTRING         = $5;
 ASN1_ITYPE_NDEF_SEQUENCE   = $6;


{$ENDREGION}
{$REGION 'BIO'}
const
  BIO_F_ACPT_STATE            = 100;
  BIO_F_BIO_ACCEPT            = 101;
  BIO_F_BIO_BER_GET_HEADER    = 102;
  BIO_F_BIO_CALLBACK_CTRL     = 131;
  BIO_F_BIO_CTRL              = 103;
  BIO_F_BIO_GETHOSTBYNAME     = 120;
  BIO_F_BIO_GETS              = 104;
  BIO_F_BIO_GET_ACCEPT_SOCKET = 105;
  BIO_F_BIO_GET_HOST_IP       = 106;
  BIO_F_BIO_GET_PORT          = 107;
  BIO_F_BIO_MAKE_PAIR         = 121;
  BIO_F_BIO_NEW               = 108;
  BIO_F_BIO_NEW_FILE          = 109;
  BIO_F_BIO_NEW_MEM_BUF       = 126;
  BIO_F_BIO_NREAD             = 123;
  BIO_F_BIO_NREAD0            = 124;
  BIO_F_BIO_NWRITE            = 125;
  BIO_F_BIO_NWRITE0           = 122;
  BIO_F_BIO_PUTS              = 110;
  BIO_F_BIO_READ              = 111;
  BIO_F_BIO_SOCK_INIT         = 112;
  BIO_F_BIO_WRITE             = 113;
  BIO_F_BUFFER_CTRL           = 114;
  BIO_F_CONN_CTRL             = 127;
  BIO_F_CONN_STATE            = 115;
  BIO_F_DGRAM_SCTP_READ       = 132;
  BIO_F_FILE_CTRL             = 116;
  BIO_F_FILE_READ             = 130;
  BIO_F_LINEBUFFER_CTRL       = 129;
  BIO_F_MEM_READ              = 128;
  BIO_F_MEM_WRITE             = 117;
  BIO_F_SSL_NEW               = 118;
  BIO_F_WSASTARTUP            = 119;
  BIO_R_ACCEPT_ERROR                          = 100;
  BIO_R_BAD_FOPEN_MODE                        = 101;
  BIO_R_BAD_HOSTNAME_LOOKUP                   = 102;
  BIO_R_BROKEN_PIPE                           = 124;
  BIO_R_CONNECT_ERROR                         = 103;
  BIO_R_EOF_ON_MEMORY_BIO                     = 127;
  BIO_R_ERROR_SETTING_NBIO                    = 104;
  BIO_R_ERROR_SETTING_NBIO_ON_ACCEPTED_SOCKET = 105;
  BIO_R_ERROR_SETTING_NBIO_ON_ACCEPT_SOCKET   = 106;
  BIO_R_GETHOSTBYNAME_ADDR_IS_NOT_AF_INET     = 107;
  BIO_R_INVALID_ARGUMENT                      = 125;
  BIO_R_INVALID_IP_ADDRESS                    = 108;
  BIO_R_IN_USE                                = 123;
  BIO_R_KEEPALIVE                             = 109;
  BIO_R_NBIO_CONNECT_ERROR                    = 110;
  BIO_R_NO_ACCEPT_PORT_SPECIFIED              = 111;
  BIO_R_NO_HOSTNAME_SPECIFIED                 = 112;
  BIO_R_NO_PORT_DEFINED                       = 113;
  BIO_R_NO_PORT_SPECIFIED                     = 114;
  BIO_R_NO_SUCH_FILE                          = 128;
  BIO_R_NULL_PARAMETER                        = 115;
  BIO_R_TAG_MISMATCH                          = 116;
  BIO_R_UNABLE_TO_BIND_SOCKET                 = 117;
  BIO_R_UNABLE_TO_CREATE_SOCKET               = 118;
  BIO_R_UNABLE_TO_LISTEN_SOCKET               = 119;
  BIO_R_UNINITIALIZED                         = 120;
  BIO_R_UNSUPPORTED_METHOD                    = 121;
  BIO_R_WRITE_TO_READ_ONLY_BIO                = 126;
  BIO_R_WSASTARTUP                            = 122;
  BIO_BIND_NORMAL                 = 0;
  BIO_BIND_REUSEADDR              = 2;
  BIO_BIND_REUSEADDR_IF_UNUSED    = 1;
  BIO_CB_CTRL                     = $06;
  BIO_CB_FREE                     = $01;
  BIO_CB_GETS                     = $05;
  BIO_CB_PUTS                     = $04;
  BIO_CB_READ                     = $02;
  BIO_CB_RETURN                   = $80;
  BIO_CB_WRITE                    = $03;
  BIO_CLOSE                       = $01;
  BIO_CONN_S_BEFORE               = 1;
  BIO_CONN_S_BLOCKED_CONNECT      = 7;
  BIO_CONN_S_CONNECT              = 5;
  BIO_CONN_S_CREATE_SOCKET        = 4;
  BIO_CONN_S_GET_IP               = 2;
  BIO_CONN_S_GET_PORT             = 3;
  BIO_CONN_S_NBIO                 = 8;
  BIO_CONN_S_OK                   = 6;
  BIO_CTRL_DUP                    = 12;
  BIO_CTRL_EOF                    = 2;
  BIO_CTRL_FLUSH                  = 11;
  BIO_CTRL_GET                    = 5;
  BIO_CTRL_GET_CALLBACK           = 15;
  BIO_CTRL_GET_CLOSE              = 8;
  BIO_CTRL_INFO                   = 3;
  BIO_CTRL_PENDING                = 10;
  BIO_CTRL_POP                    = 7;
  BIO_CTRL_PUSH                   = 6;
  BIO_CTRL_RESET                  = 1;
  BIO_CTRL_SET                    = 4;
  BIO_CTRL_SET_CALLBACK           = 14;
  BIO_CTRL_SET_CLOSE              = 9;
  BIO_CTRL_SET_FILENAME           = 30;
  BIO_CTRL_DGRAM_CONNECT          = 31;
  BIO_CTRL_DGRAM_SET_CONNECTED    = 32;
  BIO_CTRL_DGRAM_SET_RECV_TIMEOUT = 33; //* setsockopt, essentially */
  BIO_CTRL_DGRAM_GET_RECV_TIMEOUT = 34; //* getsockopt, essentially */
  BIO_CTRL_DGRAM_SET_SEND_TIMEOUT = 35; //* setsockopt, essentially */
  BIO_CTRL_DGRAM_GET_SEND_TIMEOUT = 36; //* getsockopt, essentially */
  BIO_CTRL_DGRAM_GET_RECV_TIMER_EXP = 37; //* flag whether the last */
  BIO_CTRL_DGRAM_GET_SEND_TIMER_EXP = 38; //* I/O operation tiemd out */
  BIO_CTRL_DGRAM_MTU_DISCOVER       = 39; //* set DF bit on egress packets */
  BIO_CTRL_DGRAM_QUERY_MTU          = 40; //* as kernel for current MTU */
  BIO_CTRL_DGRAM_GET_MTU            = 41; //* get cached value for MTU */
  BIO_CTRL_DGRAM_SET_MTU            = 42; //* set cached value for
  BIO_CTRL_DGRAM_MTU_EXCEEDED       = 43; //* check whether the MTU
  BIO_CTRL_DGRAM_GET_PEER           = 46;
  BIO_CTRL_DGRAM_SET_PEER           = 44; //* Destination for the data */
  BIO_CTRL_DGRAM_SET_NEXT_TIMEOUT   = 45; //* Next DTLS handshake timeout to
  BIO_CTRL_WPENDING                 = 13;
  BIO_C_DESTROY_BIO_PAIR            = 139;
  BIO_C_DO_STATE_MACHINE            = 101;
  BIO_C_FILE_SEEK                   = 128;
  BIO_C_FILE_TELL                   = 133;
  BIO_C_GET_ACCEPT                  = 124;
  BIO_C_GET_BIND_MODE               = 132;
  BIO_C_GET_BUFF_NUM_LINES          = 116;
  BIO_C_GET_BUF_MEM_PTR             = 115;
  BIO_C_GET_CIPHER_CTX              = 129;
  BIO_C_GET_CIPHER_STATUS           = 113;
  BIO_C_GET_CONNECT                 = 123;
  BIO_C_GET_FD                      = 105;
  BIO_C_GET_FILE_PTR                = 107;
  BIO_C_GET_MD                      = 112;
  BIO_C_GET_MD_CTX                  = 120;
  BIO_C_GET_PROXY_PARAM             = 121;
  BIO_C_GET_READ_REQUEST            = 141;
  BIO_C_GET_SOCKS                   = 134;
  BIO_C_GET_SSL                     = 110;
  BIO_C_GET_SSL_NUM_RENEGOTIATES    = 126;
  BIO_C_GET_WRITE_BUF_SIZE          = 137;
  BIO_C_GET_WRITE_GUARANTEE         = 140;
  BIO_C_MAKE_BIO_PAIR               = 138;
  BIO_C_SET_ACCEPT                  = 118;
  BIO_C_SET_BIND_MODE               = 131;
  BIO_C_SET_BUFF_READ_DATA          = 122;
  BIO_C_SET_BUFF_SIZE               = 117;
  BIO_C_SET_BUF_MEM                 = 114;
  BIO_C_SET_BUF_MEM_EOF_RETURN      = 130;
  BIO_C_SET_CONNECT                 = 100;
  BIO_C_SET_FD                      = 104;
  BIO_C_SET_FILENAME                = 108;
  BIO_C_SET_FILE_PTR                = 106;
  BIO_C_SET_MD                      = 111;
  BIO_C_SET_NBIO                    = 102;
  BIO_C_SET_PROXY_PARAM             = 103;
  BIO_C_SET_SOCKS                   = 135;
  BIO_C_SET_SSL                     = 109;
  BIO_C_SET_SSL_RENEGOTIATE_BYTES   = 125;
  BIO_C_SET_SSL_RENEGOTIATE_TIMEOUT = 127;
  BIO_C_SET_WRITE_BUF_SIZE          = 136;
  BIO_C_SHUTDOWN_WR                 = 142;
  BIO_C_SSL_MODE                    = 119;
  BIO_FLAGS_BASE64_NO_NL            = $100;
  BIO_FLAGS_IO_SPECIAL              = $04;
  BIO_FLAGS_READ                    = $01;
  BIO_FLAGS_WRITE                   = $02;
  BIO_FLAGS_RWS                     = BIO_FLAGS_READ or
                          BIO_FLAGS_WRITE or
                          BIO_FLAGS_IO_SPECIAL;
  BIO_FLAGS_SHOULD_RETRY            = $08;
  BIO_FP_APPEND                     = $08;
  BIO_FP_READ                       = $02;
  BIO_FP_TEXT                       = $10;
  BIO_FP_WRITE                      = $04;
  BIO_GHBN_CTRL_CACHE_SIZE          = 3;
  BIO_GHBN_CTRL_FLUSH               = 5;
  BIO_GHBN_CTRL_GET_ENTRY           = 4;
  BIO_GHBN_CTRL_HITS                = 1;
  BIO_GHBN_CTRL_MISSES              = 2;
  BIO_NOCLOSE                       = $00;
  BIO_RR_CONNECT                    = $02;
  BIO_RR_SSL_X509_LOOKUP            = $01;
  BIO_TYPE_ACCEPT                   = 13 or $0400 or $0100;
  BIO_TYPE_BASE64                   = 11 or $0200;
  BIO_TYPE_BER                      = 18 or $0200;
  BIO_TYPE_BIO                      = 19 or $0400;
  BIO_TYPE_BUFFER                   = 9 or $0200;
  BIO_TYPE_CIPHER                   = 10 or $0200;
  BIO_TYPE_CONNECT                  = 12 or $0400 or $0100;
  BIO_TYPE_DESCRIPTOR               = $0100;
  BIO_TYPE_FD                       = 4 or $0400 or $0100;
  BIO_TYPE_FILE                     = 2 or $0400;
  BIO_TYPE_FILTER                   = $0200;
  BIO_TYPE_MD                       = 8 or $0200;
  BIO_TYPE_MEM                      = 1 or $0400;
  BIO_TYPE_NBIO_TEST                = 16 or $0200;
  BIO_TYPE_NONE                     = 0;
  BIO_TYPE_NULL                     = 6 or $0400;
  BIO_TYPE_NULL_FILTER              = 17 or $0200;
  BIO_TYPE_PROXY_CLIENT             = 14 or $0200;
  BIO_TYPE_PROXY_SERVER             = 15 or $0200;
  BIO_TYPE_SOCKET                   = 5 or $0400 or $0100;
  BIO_TYPE_SOURCE_SINK              = $0400;
  BIO_TYPE_SSL                      = 7 or $0200;
  BIO_TYPE_LINEBUFFER               = 20 or $0200;
  BIO_TYPE_DGRAM                    = 21 or $0400 or $0100;
  BIO_TYPE_COMP                     = 23 or $0200;
{$ENDREGION}
{$REGION 'BN'}
 BN_BITS        = 128;
 BN_BYTES       = 8;
 BN_BITS2       = 64;
 BN_BITS4       = 32;
 BN_MASK2       = $ffffffffffffffff;
 BN_MASK2l      = $ffffffff;
 BN_MASK2h      = $ffffffff00000000;
 BN_MASK2h1     = $ffffffff80000000;
 BN_TBIT        = $8000000000000000;
 BN_DEC_CONV    = 10000000000000000000;
 BN_DEC_FMT1    = '%lu';
 BN_DEC_FMT2    = '%019lu';
 BN_DEC_NUM     = 19;
 BN_HEX_FMT1    = '%lX';
 BN_HEX_FMT2    = '%016lX';
 BN_CTX_POOL_SIZE   = 16;
 BN_CTX_START_FRAMES    = 32;
 BN_F_BNRAND                                 = 127;
 BN_F_BN_BLINDING_CONVERT_EX                 = 100;
 BN_F_BN_BLINDING_CREATE_PARAM               = 128;
 BN_F_BN_BLINDING_INVERT_EX                  = 101;
 BN_F_BN_BLINDING_NEW                        = 102;
 BN_F_BN_BLINDING_UPDATE                     = 103;
 BN_F_BN_BN2DEC                              = 104;
 BN_F_BN_BN2HEX                              = 105;
 BN_F_BN_CTX_GET                             = 116;
 BN_F_BN_CTX_NEW                             = 106;
 BN_F_BN_CTX_START                           = 129;
 BN_F_BN_DIV                                 = 107;
 BN_F_BN_DIV_NO_BRANCH                       = 138;
 BN_F_BN_DIV_RECP                            = 130;
 BN_F_BN_EXP                                 = 123;
 BN_F_BN_EXPAND2                             = 108;
 BN_F_BN_EXPAND_INTERNAL                     = 120;
 BN_F_BN_GF2M_MOD                            = 131;
 BN_F_BN_GF2M_MOD_EXP                        = 132;
 BN_F_BN_GF2M_MOD_MUL                        = 133;
 BN_F_BN_GF2M_MOD_SOLVE_QUAD                 = 134;
 BN_F_BN_GF2M_MOD_SOLVE_QUAD_ARR             = 135;
 BN_F_BN_GF2M_MOD_SQR                        = 136;
 BN_F_BN_GF2M_MOD_SQRT                       = 137;
 BN_F_BN_MOD_EXP2_MONT                       = 118;
 BN_F_BN_MOD_EXP_MONT                        = 109;
 BN_F_BN_MOD_EXP_MONT_CONSTTIME              = 124;
 BN_F_BN_MOD_EXP_MONT_WORD                   = 117;
 BN_F_BN_MOD_EXP_RECP                        = 125;
 BN_F_BN_MOD_EXP_SIMPLE                      = 126;
 BN_F_BN_MOD_INVERSE                         = 110;
 BN_F_BN_MOD_INVERSE_NO_BRANCH               = 139;
 BN_F_BN_MOD_LSHIFT_QUICK                    = 119;
 BN_F_BN_MOD_MUL_RECIPROCAL                  = 111;
 BN_F_BN_MOD_SQRT                            = 121;
 BN_F_BN_MPI2BN                              = 112;
 BN_F_BN_NEW                                 = 113;
 BN_F_BN_RAND                                = 114;
 BN_F_BN_RAND_RANGE                          = 122;
 BN_F_BN_USUB                                = 115;

 BN_R_ARG2_LT_ARG3                       = 100;
 BN_R_BAD_RECIPROCAL                     = 101;
 BN_R_BIGNUM_TOO_LONG                    = 114;
 BN_R_CALLED_WITH_EVEN_MODULUS           = 102;
 BN_R_DIV_BY_ZERO                        = 103;
 BN_R_ENCODING_ERROR                     = 104;
 BN_R_EXPAND_ON_STATIC_BIGNUM_DATA       = 105;
 BN_R_INPUT_NOT_REDUCED                  = 110;
 BN_R_INVALID_LENGTH                     = 106;
 BN_R_INVALID_RANGE                      = 115;
 BN_R_NOT_A_SQUARE                       = 111;
 BN_R_NOT_INITIALIZED                    = 107;
 BN_R_NO_INVERSE                         = 108;
 BN_R_NO_SOLUTION                        = 116;
 BN_R_P_IS_NOT_PRIME                     = 112;
 BN_R_TOO_MANY_ITERATIONS                = 113;
 BN_R_TOO_MANY_TEMPORARY_VARIABLES       = 109;

{$ENDREGION}
{$REGION 'BUFFER'}
 BUF_F_BUF_MEMDUP                    = 103;
 BUF_F_BUF_MEM_GROW                  = 100;
 BUF_F_BUF_MEM_GROW_CLEAN            = 105;
 BUF_F_BUF_MEM_NEW                   = 101;
 BUF_F_BUF_STRDUP                    = 102;
 BUF_F_BUF_STRNDUP                   = 104;
{$ENDREGION}
{$REGION 'CMS'}
 CMS_SIGNERINFO_ISSUER_SERIAL   = 0;
 CMS_SIGNERINFO_KEYIDENTIFIER   = 1;
 CMS_RECIPINFO_TRANS        = 0;
 CMS_RECIPINFO_AGREE        = 1;
 CMS_RECIPINFO_KEK          = 2;
 CMS_RECIPINFO_PASS         = 3;
 CMS_RECIPINFO_OTHER        = 4;
 CMS_TEXT                   = $1;
 CMS_NOCERTS                = $2;
 CMS_NO_CONTENT_VERIFY      = $4;
 CMS_NO_ATTR_VERIFY         = $8;
 CMS_NOSIGS                 = (CMS_NO_CONTENT_VERIFY or CMS_NO_ATTR_VERIFY);
 CMS_NOINTERN               = $10;
 CMS_NO_SIGNER_CERT_VERIFY  = $20;
 CMS_NOVERIFY               = $20;
 CMS_DETACHED               = $40;
 CMS_BINARY                 = $80;
 CMS_NOATTR                 = $100;
 CMS_NOSMIMECAP             = $200;
 CMS_NOOLDMIMETYPE          = $400;
 CMS_CRLFEOL                = $800;
 CMS_STREAM                 = $1000;
 CMS_NOCRL                  = $2000;
 CMS_PARTIAL                = $4000;
 CMS_REUSE_DIGEST           = $8000;
 CMS_USE_KEYID              = $10000;
 CMS_DEBUG_DECRYPT          = $20000;
 CMS_F_CHECK_CONTENT                             = 99;
 CMS_F_CMS_ADD0_CERT                             = 164;
 CMS_F_CMS_ADD0_RECIPIENT_KEY                    = 100;
 CMS_F_CMS_ADD0_RECIPIENT_PASSWORD               = 165;
 CMS_F_CMS_ADD1_RECEIPTREQUEST                   = 158;
 CMS_F_CMS_ADD1_RECIPIENT_CERT                   = 101;
 CMS_F_CMS_ADD1_SIGNER                           = 102;
 CMS_F_CMS_ADD1_SIGNINGTIME                      = 103;
 CMS_F_CMS_COMPRESS                              = 104;
 CMS_F_CMS_COMPRESSEDDATA_CREATE                 = 105;
 CMS_F_CMS_COMPRESSEDDATA_INIT_BIO               = 106;
 CMS_F_CMS_COPY_CONTENT                          = 107;
 CMS_F_CMS_COPY_MESSAGEDIGEST                    = 108;
 CMS_F_CMS_DATA                                  = 109;
 CMS_F_CMS_DATAFINAL                             = 110;
 CMS_F_CMS_DATAINIT                              = 111;
 CMS_F_CMS_DECRYPT                               = 112;
 CMS_F_CMS_DECRYPT_SET1_KEY                      = 113;
 CMS_F_CMS_DECRYPT_SET1_PASSWORD                 = 166;
 CMS_F_CMS_DECRYPT_SET1_PKEY                     = 114;
 CMS_F_CMS_DIGESTALGORITHM_FIND_CTX              = 115;
 CMS_F_CMS_DIGESTALGORITHM_INIT_BIO              = 116;
 CMS_F_CMS_DIGESTEDDATA_DO_FINAL                 = 117;
 CMS_F_CMS_DIGEST_VERIFY                         = 118;
 CMS_F_CMS_ENCODE_RECEIPT                        = 161;
 CMS_F_CMS_ENCRYPT                               = 119;
 CMS_F_CMS_ENCRYPTEDCONTENT_INIT_BIO             = 120;
 CMS_F_CMS_ENCRYPTEDDATA_DECRYPT                 = 121;
 CMS_F_CMS_ENCRYPTEDDATA_ENCRYPT                 = 122;
 CMS_F_CMS_ENCRYPTEDDATA_SET1_KEY                = 123;
 CMS_F_CMS_ENVELOPEDDATA_CREATE                  = 124;
 CMS_F_CMS_ENVELOPEDDATA_INIT_BIO                = 125;
 CMS_F_CMS_ENVELOPED_DATA_INIT                   = 126;
 CMS_F_CMS_FINAL                                 = 127;
 CMS_F_CMS_GET0_CERTIFICATE_CHOICES              = 128;
 CMS_F_CMS_GET0_CONTENT                          = 129;
 CMS_F_CMS_GET0_ECONTENT_TYPE                    = 130;
 CMS_F_CMS_GET0_ENVELOPED                        = 131;
 CMS_F_CMS_GET0_REVOCATION_CHOICES               = 132;
 CMS_F_CMS_GET0_SIGNED                           = 133;
 CMS_F_CMS_MSGSIGDIGEST_ADD1                     = 162;
 CMS_F_CMS_RECEIPTREQUEST_CREATE0                = 159;
 CMS_F_CMS_RECEIPT_VERIFY                        = 160;
 CMS_F_CMS_RECIPIENTINFO_DECRYPT                 = 134;
 CMS_F_CMS_RECIPIENTINFO_KEKRI_DECRYPT           = 135;
 CMS_F_CMS_RECIPIENTINFO_KEKRI_ENCRYPT           = 136;
 CMS_F_CMS_RECIPIENTINFO_KEKRI_GET0_ID           = 137;
 CMS_F_CMS_RECIPIENTINFO_KEKRI_ID_CMP            = 138;
 CMS_F_CMS_RECIPIENTINFO_KTRI_CERT_CMP           = 139;
 CMS_F_CMS_RECIPIENTINFO_KTRI_DECRYPT            = 140;
 CMS_F_CMS_RECIPIENTINFO_KTRI_ENCRYPT            = 141;
 CMS_F_CMS_RECIPIENTINFO_KTRI_GET0_ALGS          = 142;
 CMS_F_CMS_RECIPIENTINFO_KTRI_GET0_SIGNER_ID     = 143;
 CMS_F_CMS_RECIPIENTINFO_PWRI_CRYPT              = 167;
 CMS_F_CMS_RECIPIENTINFO_SET0_KEY                = 144;
 CMS_F_CMS_RECIPIENTINFO_SET0_PASSWORD           = 168;
 CMS_F_CMS_RECIPIENTINFO_SET0_PKEY               = 145;
 CMS_F_CMS_SET1_SIGNERIDENTIFIER                 = 146;
 CMS_F_CMS_SET_DETACHED                          = 147;
 CMS_F_CMS_SIGN                                  = 148;
 CMS_F_CMS_SIGNED_DATA_INIT                      = 149;
 CMS_F_CMS_SIGNERINFO_CONTENT_SIGN               = 150;
 CMS_F_CMS_SIGNERINFO_SIGN                       = 151;
 CMS_F_CMS_SIGNERINFO_VERIFY                     = 152;
 CMS_F_CMS_SIGNERINFO_VERIFY_CERT                = 153;
 CMS_F_CMS_SIGNERINFO_VERIFY_CONTENT             = 154;
 CMS_F_CMS_SIGN_RECEIPT                          = 163;
 CMS_F_CMS_STREAM                                = 155;
 CMS_F_CMS_UNCOMPRESS                            = 156;
 CMS_F_CMS_VERIFY                                = 157;
 CMS_R_ADD_SIGNER_ERROR                          = 99;
 CMS_R_CERTIFICATE_ALREADY_PRESENT               = 175;
 CMS_R_CERTIFICATE_HAS_NO_KEYID                  = 160;
 CMS_R_CERTIFICATE_VERIFY_ERROR                  = 100;
 CMS_R_CIPHER_INITIALISATION_ERROR               = 101;
 CMS_R_CIPHER_PARAMETER_INITIALISATION_ERROR     = 102;
 CMS_R_CMS_DATAFINAL_ERROR                       = 103;
 CMS_R_CMS_LIB                                   = 104;
 CMS_R_CONTENTIDENTIFIER_MISMATCH                = 170;
 CMS_R_CONTENT_NOT_FOUND                         = 105;
 CMS_R_CONTENT_TYPE_MISMATCH                     = 171;
 CMS_R_CONTENT_TYPE_NOT_COMPRESSED_DATA          = 106;
 CMS_R_CONTENT_TYPE_NOT_ENVELOPED_DATA           = 107;
 CMS_R_CONTENT_TYPE_NOT_SIGNED_DATA              = 108;
 CMS_R_CONTENT_VERIFY_ERROR                      = 109;
 CMS_R_CTRL_ERROR                                = 110;
 CMS_R_CTRL_FAILURE                              = 111;
 CMS_R_DECRYPT_ERROR                             = 112;
 CMS_R_DIGEST_ERROR                              = 161;
 CMS_R_ERROR_GETTING_PUBLIC_KEY                  = 113;
 CMS_R_ERROR_READING_MESSAGEDIGEST_ATTRIBUTE     = 114;
 CMS_R_ERROR_SETTING_KEY                         = 115;
 CMS_R_ERROR_SETTING_RECIPIENTINFO               = 116;
 CMS_R_INVALID_ENCRYPTED_KEY_LENGTH              = 117;
 CMS_R_INVALID_KEY_ENCRYPTION_PARAMETER          = 176;
 CMS_R_INVALID_KEY_LENGTH                        = 118;
 CMS_R_MD_BIO_INIT_ERROR                         = 119;
 CMS_R_MESSAGEDIGEST_ATTRIBUTE_WRONG_LENGTH      = 120;
 CMS_R_MESSAGEDIGEST_WRONG_LENGTH                = 121;
 CMS_R_MSGSIGDIGEST_ERROR                        = 172;
 CMS_R_MSGSIGDIGEST_VERIFICATION_FAILURE         = 162;
 CMS_R_MSGSIGDIGEST_WRONG_LENGTH                 = 163;
 CMS_R_NEED_ONE_SIGNER                           = 164;
 CMS_R_NOT_A_SIGNED_RECEIPT                      = 165;
 CMS_R_NOT_ENCRYPTED_DATA                        = 122;
 CMS_R_NOT_KEK                                   = 123;
 CMS_R_NOT_KEY_TRANSPORT                         = 124;
 CMS_R_NOT_PWRI                                  = 177;
 CMS_R_NOT_SUPPORTED_FOR_THIS_KEY_TYPE           = 125;
 CMS_R_NO_CIPHER                                 = 126;
 CMS_R_NO_CONTENT                                = 127;
 CMS_R_NO_CONTENT_TYPE                           = 173;
 CMS_R_NO_DEFAULT_DIGEST                         = 128;
 CMS_R_NO_DIGEST_SET                             = 129;
 CMS_R_NO_KEY                                    = 130;
 CMS_R_NO_KEY_OR_CERT                            = 174;
 CMS_R_NO_MATCHING_DIGEST                        = 131;
 CMS_R_NO_MATCHING_RECIPIENT                     = 132;
 CMS_R_NO_MATCHING_SIGNATURE                     = 166;
 CMS_R_NO_MSGSIGDIGEST                           = 167;
 CMS_R_NO_PASSWORD                               = 178;
 CMS_R_NO_PRIVATE_KEY                            = 133;
 CMS_R_NO_PUBLIC_KEY                             = 134;
 CMS_R_NO_RECEIPT_REQUEST                        = 168;
 CMS_R_NO_SIGNERS                                = 135;
 CMS_R_PRIVATE_KEY_DOES_NOT_MATCH_CERTIFICATE    = 136;
 CMS_R_RECEIPT_DECODE_ERROR                      = 169;
 CMS_R_RECIPIENT_ERROR                           = 137;
 CMS_R_SIGNER_CERTIFICATE_NOT_FOUND              = 138;
 CMS_R_SIGNFINAL_ERROR                           = 139;
 CMS_R_SMIME_TEXT_ERROR                          = 140;
 CMS_R_STORE_INIT_ERROR                          = 141;
 CMS_R_TYPE_NOT_COMPRESSED_DATA                  = 142;
 CMS_R_TYPE_NOT_DATA                             = 143;
 CMS_R_TYPE_NOT_DIGESTED_DATA                    = 144;
 CMS_R_TYPE_NOT_ENCRYPTED_DATA                   = 145;
 CMS_R_TYPE_NOT_ENVELOPED_DATA                   = 146;
 CMS_R_UNABLE_TO_FINALIZE_CONTEXT                = 147;
 CMS_R_UNKNOWN_CIPHER                            = 148;
 CMS_R_UNKNOWN_DIGEST_ALGORIHM                   = 149;
 CMS_R_UNKNOWN_ID                                = 150;
 CMS_R_UNSUPPORTED_COMPRESSION_ALGORITHM         = 151;
 CMS_R_UNSUPPORTED_CONTENT_TYPE                  = 152;
 CMS_R_UNSUPPORTED_KEK_ALGORITHM                 = 153;
 CMS_R_UNSUPPORTED_KEY_ENCRYPTION_ALGORITHM      = 179;
 CMS_R_UNSUPPORTED_RECIPIENT_TYPE                = 154;
 CMS_R_UNSUPPORTED_RECPIENTINFO_TYPE             = 155;
 CMS_R_UNSUPPORTED_TYPE                          = 156;
 CMS_R_UNWRAP_ERROR                              = 157;
 CMS_R_UNWRAP_FAILURE                            = 180;
 CMS_R_VERIFICATION_FAILURE                      = 158;
 CMS_R_WRAP_ERROR                                = 159;
{$ENDREGION}
{$REGION 'DSA'}
const
 DSA_F_D2I_DSA_SIG                       = 110;
 DSA_F_DO_DSA_PRINT                      = 104;
 DSA_F_DSAPARAMS_PRINT                   = 100;
 DSA_F_DSAPARAMS_PRINT_FP                = 101;
 DSA_F_DSA_DO_SIGN                       = 112;
 DSA_F_DSA_DO_VERIFY                     = 113;
 DSA_F_DSA_GENERATE_KEY                  = 124;
 DSA_F_DSA_GENERATE_PARAMETERS_EX        = 123;
 DSA_F_DSA_NEW_METHOD                    = 103;
 DSA_F_DSA_PARAM_DECODE                  = 119;
 DSA_F_DSA_PRINT_FP                      = 105;
 DSA_F_DSA_PRIV_DECODE                   = 115;
 DSA_F_DSA_PRIV_ENCODE                   = 116;
 DSA_F_DSA_PUB_DECODE                    = 117;
 DSA_F_DSA_PUB_ENCODE                    = 118;
 DSA_F_DSA_SIGN                          = 106;
 DSA_F_DSA_SIGN_SETUP                    = 107;
 DSA_F_DSA_SIG_NEW                       = 109;
 DSA_F_DSA_SIG_PRINT                     = 125;
 DSA_F_DSA_VERIFY                        = 108;
 DSA_F_I2D_DSA_SIG                       = 111;
 DSA_F_OLD_DSA_PRIV_DECODE               = 122;
 DSA_F_PKEY_DSA_CTRL                     = 120;
 DSA_F_PKEY_DSA_KEYGEN                   = 121;
 DSA_F_SIG_CB                            = 114;

 DSA_R_BAD_Q_VALUE                       = 102;
 DSA_R_BN_DECODE_ERROR                   = 108;
 DSA_R_BN_ERROR                          = 109;
 DSA_R_DATA_TOO_LARGE_FOR_KEY_SIZE       = 100;
 DSA_R_DECODE_ERROR                      = 104;
 DSA_R_INVALID_DIGEST_TYPE               = 106;
 DSA_R_MISSING_PARAMETERS                = 101;
 DSA_R_MODULUS_TOO_LARGE                 = 103;
 DSA_R_NEED_NEW_SETUP_VALUES             = 110;
 DSA_R_NON_FIPS_DSA_METHOD               = 111;
 DSA_R_NO_PARAMETERS_SET                 = 107;
 DSA_R_PARAMETER_ENCODING_ERROR          = 105;
{$ENDREGION}
{$REGION 'EC'}
const
  POINT_CONVERSION_COMPRESSED = 2;
  POINT_CONVERSION_UNCOMPRESSED = 4;
  POINT_CONVERSION_HYBRID = 6;
 EC_F_BN_TO_FELEM                                    = 224;
 EC_F_COMPUTE_WNAF                                   = 143;
 EC_F_D2I_ECPARAMETERS                               = 144;
 EC_F_D2I_ECPKPARAMETERS                             = 145;
 EC_F_D2I_ECPRIVATEKEY                               = 146;
 EC_F_DO_EC_KEY_PRINT                                = 221;
 EC_F_ECKEY_PARAM2TYPE                               = 223;
 EC_F_ECKEY_PARAM_DECODE                             = 212;
 EC_F_ECKEY_PRIV_DECODE                              = 213;
 EC_F_ECKEY_PRIV_ENCODE                              = 214;
 EC_F_ECKEY_PUB_DECODE                               = 215;
 EC_F_ECKEY_PUB_ENCODE                               = 216;
 EC_F_ECKEY_TYPE2PARAM                               = 220;
 EC_F_ECPARAMETERS_PRINT                             = 147;
 EC_F_ECPARAMETERS_PRINT_FP                          = 148;
 EC_F_ECPKPARAMETERS_PRINT                           = 149;
 EC_F_ECPKPARAMETERS_PRINT_FP                        = 150;
 EC_F_ECP_NIST_MOD_192                               = 203;
 EC_F_ECP_NIST_MOD_224                               = 204;
 EC_F_ECP_NIST_MOD_256                               = 205;
 EC_F_ECP_NIST_MOD_521                               = 206;
 EC_F_EC_ASN1_GROUP2CURVE                            = 153;
 EC_F_EC_ASN1_GROUP2FIELDID                          = 154;
 EC_F_EC_ASN1_GROUP2PARAMETERS                       = 155;
 EC_F_EC_ASN1_GROUP2PKPARAMETERS                     = 156;
 EC_F_EC_ASN1_PARAMETERS2GROUP                       = 157;
 EC_F_EC_ASN1_PKPARAMETERS2GROUP                     = 158;
 EC_F_EC_EX_DATA_SET_DATA                            = 211;
 EC_F_EC_GF2M_MONTGOMERY_POINT_MULTIPLY              = 208;
 EC_F_EC_GF2M_SIMPLE_GROUP_CHECK_DISCRIMINANT        = 159;
 EC_F_EC_GF2M_SIMPLE_GROUP_SET_CURVE                 = 195;
 EC_F_EC_GF2M_SIMPLE_OCT2POINT                       = 160;
 EC_F_EC_GF2M_SIMPLE_POINT2OCT                       = 161;
 EC_F_EC_GF2M_SIMPLE_POINT_GET_AFFINE_COORDINATES    = 162;
 EC_F_EC_GF2M_SIMPLE_POINT_SET_AFFINE_COORDINATES    = 163;
 EC_F_EC_GF2M_SIMPLE_SET_COMPRESSED_COORDINATES      = 164;
 EC_F_EC_GFP_MONT_FIELD_DECODE                       = 133;
 EC_F_EC_GFP_MONT_FIELD_ENCODE                       = 134;
 EC_F_EC_GFP_MONT_FIELD_MUL                          = 131;
 EC_F_EC_GFP_MONT_FIELD_SET_TO_ONE                   = 209;
 EC_F_EC_GFP_MONT_FIELD_SQR                          = 132;
 EC_F_EC_GFP_MONT_GROUP_SET_CURVE                    = 189;
 EC_F_EC_GFP_MONT_GROUP_SET_CURVE_GFP                = 135;
 EC_F_EC_GFP_NISTP224_GROUP_SET_CURVE                = 225;
 EC_F_EC_GFP_NISTP224_POINTS_MUL                     = 228;
 EC_F_EC_GFP_NISTP224_POINT_GET_AFFINE_COORDINATES   = 226;
 EC_F_EC_GFP_NISTP256_GROUP_SET_CURVE                = 230;
 EC_F_EC_GFP_NISTP256_POINTS_MUL                     = 231;
 EC_F_EC_GFP_NISTP256_POINT_GET_AFFINE_COORDINATES   = 232;
 EC_F_EC_GFP_NISTP521_GROUP_SET_CURVE                = 233;
 EC_F_EC_GFP_NISTP521_POINTS_MUL                     = 234;
 EC_F_EC_GFP_NISTP521_POINT_GET_AFFINE_COORDINATES   = 235;
 EC_F_EC_GFP_NIST_FIELD_MUL                          = 200;
 EC_F_EC_GFP_NIST_FIELD_SQR                          = 201;
 EC_F_EC_GFP_NIST_GROUP_SET_CURVE                    = 202;
 EC_F_EC_GFP_SIMPLE_GROUP_CHECK_DISCRIMINANT         = 165;
 EC_F_EC_GFP_SIMPLE_GROUP_SET_CURVE                  = 166;
 EC_F_EC_GFP_SIMPLE_GROUP_SET_CURVE_GFP              = 100;
 EC_F_EC_GFP_SIMPLE_GROUP_SET_GENERATOR              = 101;
 EC_F_EC_GFP_SIMPLE_MAKE_AFFINE                      = 102;
 EC_F_EC_GFP_SIMPLE_OCT2POINT                        = 103;
 EC_F_EC_GFP_SIMPLE_POINT2OCT                        = 104;
 EC_F_EC_GFP_SIMPLE_POINTS_MAKE_AFFINE               = 137;
 EC_F_EC_GFP_SIMPLE_POINT_GET_AFFINE_COORDINATES     = 167;
 EC_F_EC_GFP_SIMPLE_POINT_GET_AFFINE_COORDINATES_GFP = 105;
 EC_F_EC_GFP_SIMPLE_POINT_SET_AFFINE_COORDINATES     = 168;
 EC_F_EC_GFP_SIMPLE_POINT_SET_AFFINE_COORDINATES_GFP = 128;
 EC_F_EC_GFP_SIMPLE_SET_COMPRESSED_COORDINATES       = 169;
 EC_F_EC_GFP_SIMPLE_SET_COMPRESSED_COORDINATES_GFP   = 129;
 EC_F_EC_GROUP_CHECK                                 = 170;
 EC_F_EC_GROUP_CHECK_DISCRIMINANT                    = 171;
 EC_F_EC_GROUP_COPY                                  = 106;
 EC_F_EC_GROUP_GET0_GENERATOR                        = 139;
 EC_F_EC_GROUP_GET_COFACTOR                          = 140;
 EC_F_EC_GROUP_GET_CURVE_GF2M                        = 172;
 EC_F_EC_GROUP_GET_CURVE_GFP                         = 130;
 EC_F_EC_GROUP_GET_DEGREE                            = 173;
 EC_F_EC_GROUP_GET_ORDER                             = 141;
 EC_F_EC_GROUP_GET_PENTANOMIAL_BASIS                 = 193;
 EC_F_EC_GROUP_GET_TRINOMIAL_BASIS                   = 194;
 EC_F_EC_GROUP_NEW                                   = 108;
 EC_F_EC_GROUP_NEW_BY_CURVE_NAME                     = 174;
 EC_F_EC_GROUP_NEW_FROM_DATA                         = 175;
 EC_F_EC_GROUP_PRECOMPUTE_MULT                       = 142;
 EC_F_EC_GROUP_SET_CURVE_GF2M                        = 176;
 EC_F_EC_GROUP_SET_CURVE_GFP                         = 109;
 EC_F_EC_GROUP_SET_EXTRA_DATA                        = 110;
 EC_F_EC_GROUP_SET_GENERATOR                         = 111;
 EC_F_EC_KEY_CHECK_KEY                               = 177;
 EC_F_EC_KEY_COPY                                    = 178;
 EC_F_EC_KEY_GENERATE_KEY                            = 179;
 EC_F_EC_KEY_NEW                                     = 182;
 EC_F_EC_KEY_PRINT                                   = 180;
 EC_F_EC_KEY_PRINT_FP                                = 181;
 EC_F_EC_KEY_SET_PUBLIC_KEY_AFFINE_COORDINATES       = 229;
 EC_F_EC_POINTS_MAKE_AFFINE                          = 136;
 EC_F_EC_POINT_ADD                                   = 112;
 EC_F_EC_POINT_CMP                                   = 113;
 EC_F_EC_POINT_COPY                                  = 114;
 EC_F_EC_POINT_DBL                                   = 115;
 EC_F_EC_POINT_GET_AFFINE_COORDINATES_GF2M           = 183;
 EC_F_EC_POINT_GET_AFFINE_COORDINATES_GFP            = 116;
 EC_F_EC_POINT_GET_JPROJECTIVE_COORDINATES_GFP       = 117;
 EC_F_EC_POINT_INVERT                                = 210;
 EC_F_EC_POINT_IS_AT_INFINITY                        = 118;
 EC_F_EC_POINT_IS_ON_CURVE                           = 119;
 EC_F_EC_POINT_MAKE_AFFINE                           = 120;
 EC_F_EC_POINT_MUL                                   = 184;
 EC_F_EC_POINT_NEW                                   = 121;
 EC_F_EC_POINT_OCT2POINT                             = 122;
 EC_F_EC_POINT_POINT2OCT                             = 123;
 EC_F_EC_POINT_SET_AFFINE_COORDINATES_GF2M           = 185;
 EC_F_EC_POINT_SET_AFFINE_COORDINATES_GFP            = 124;
 EC_F_EC_POINT_SET_COMPRESSED_COORDINATES_GF2M       = 186;
 EC_F_EC_POINT_SET_COMPRESSED_COORDINATES_GFP        = 125;
 EC_F_EC_POINT_SET_JPROJECTIVE_COORDINATES_GFP       = 126;
 EC_F_EC_POINT_SET_TO_INFINITY                       = 127;
 EC_F_EC_PRE_COMP_DUP                                = 207;
 EC_F_EC_PRE_COMP_NEW                                = 196;
 EC_F_EC_WNAF_MUL                                    = 187;
 EC_F_EC_WNAF_PRECOMPUTE_MULT                        = 188;
 EC_F_I2D_ECPARAMETERS                               = 190;
 EC_F_I2D_ECPKPARAMETERS                             = 191;
 EC_F_I2D_ECPRIVATEKEY                               = 192;
 EC_F_I2O_ECPUBLICKEY                                = 151;
 EC_F_NISTP224_PRE_COMP_NEW                          = 227;
 EC_F_NISTP256_PRE_COMP_NEW                          = 236;
 EC_F_NISTP521_PRE_COMP_NEW                          = 237;
 EC_F_O2I_ECPUBLICKEY                                = 152;
 EC_F_OLD_EC_PRIV_DECODE                             = 222;
 EC_F_PKEY_EC_CTRL                                   = 197;
 EC_F_PKEY_EC_CTRL_STR                               = 198;
 EC_F_PKEY_EC_DERIVE                                 = 217;
 EC_F_PKEY_EC_KEYGEN                                 = 199;
 EC_F_PKEY_EC_PARAMGEN                               = 219;
 EC_F_PKEY_EC_SIGN                                   = 218;
 EC_R_ASN1_ERROR                             = 115;
 EC_R_ASN1_UNKNOWN_FIELD                     = 116;
 EC_R_BIGNUM_OUT_OF_RANGE                    = 144;
 EC_R_BUFFER_TOO_SMALL                       = 100;
 EC_R_COORDINATES_OUT_OF_RANGE               = 146;
 EC_R_D2I_ECPKPARAMETERS_FAILURE             = 117;
 EC_R_DECODE_ERROR                           = 142;
 EC_R_DISCRIMINANT_IS_ZERO                   = 118;
 EC_R_EC_GROUP_NEW_BY_NAME_FAILURE           = 119;
 EC_R_FIELD_TOO_LARGE                        = 143;
 EC_R_GF2M_NOT_SUPPORTED                     = 147;
 EC_R_GROUP2PKPARAMETERS_FAILURE             = 120;
 EC_R_I2D_ECPKPARAMETERS_FAILURE             = 121;
 EC_R_INCOMPATIBLE_OBJECTS                   = 101;
 EC_R_INVALID_ARGUMENT                       = 112;
 EC_R_INVALID_COMPRESSED_POINT               = 110;
 EC_R_INVALID_COMPRESSION_BIT                = 109;
 EC_R_INVALID_CURVE                          = 141;
 EC_R_INVALID_DIGEST_TYPE                    = 138;
 EC_R_INVALID_ENCODING                       = 102;
 EC_R_INVALID_FIELD                          = 103;
 EC_R_INVALID_FORM                           = 104;
 EC_R_INVALID_GROUP_ORDER                    = 122;
 EC_R_INVALID_PENTANOMIAL_BASIS              = 132;
 EC_R_INVALID_PRIVATE_KEY                    = 123;
 EC_R_INVALID_TRINOMIAL_BASIS                = 137;
 EC_R_KEYS_NOT_SET                           = 140;
 EC_R_MISSING_PARAMETERS                     = 124;
 EC_R_MISSING_PRIVATE_KEY                    = 125;
 EC_R_NOT_A_NIST_PRIME                       = 135;
 EC_R_NOT_A_SUPPORTED_NIST_PRIME             = 136;
 EC_R_NOT_IMPLEMENTED                        = 126;
 EC_R_NOT_INITIALIZED                        = 111;
 EC_R_NO_FIELD_MOD                           = 133;
 EC_R_NO_PARAMETERS_SET                      = 139;
 EC_R_PASSED_NULL_PARAMETER                  = 134;
 EC_R_PKPARAMETERS2GROUP_FAILURE             = 127;
 EC_R_POINT_AT_INFINITY                      = 106;
 EC_R_POINT_IS_NOT_ON_CURVE                  = 107;
 EC_R_SLOT_FULL                              = 108;
 EC_R_UNDEFINED_GENERATOR                    = 113;
 EC_R_UNDEFINED_ORDER                        = 128;
 EC_R_UNKNOWN_GROUP                          = 129;
 EC_R_UNKNOWN_ORDER                          = 114;
 EC_R_UNSUPPORTED_FIELD                      = 131;
 EC_R_WRONG_CURVE_PARAMETERS                 = 145;
 EC_R_WRONG_ORDER                            = 130;
{$ENDREGION}
{$REGION 'ENGINE'}
const
 ENGINE_F_DYNAMIC_CTRL                       = 180;
 ENGINE_F_DYNAMIC_GET_DATA_CTX               = 181;
 ENGINE_F_DYNAMIC_LOAD                       = 182;
 ENGINE_F_DYNAMIC_SET_DATA_CTX               = 183;
 ENGINE_F_ENGINE_ADD                         = 105;
 ENGINE_F_ENGINE_BY_ID                       = 106;
 ENGINE_F_ENGINE_CMD_IS_EXECUTABLE           = 170;
 ENGINE_F_ENGINE_CTRL                        = 142;
 ENGINE_F_ENGINE_CTRL_CMD                    = 178;
 ENGINE_F_ENGINE_CTRL_CMD_STRING             = 171;
 ENGINE_F_ENGINE_FINISH                      = 107;
 ENGINE_F_ENGINE_FREE_UTIL                   = 108;
 ENGINE_F_ENGINE_GET_CIPHER                  = 185;
 ENGINE_F_ENGINE_GET_DEFAULT_TYPE            = 177;
 ENGINE_F_ENGINE_GET_DIGEST                  = 186;
 ENGINE_F_ENGINE_GET_NEXT                    = 115;
 ENGINE_F_ENGINE_GET_PKEY_ASN1_METH          = 193;
 ENGINE_F_ENGINE_GET_PKEY_METH               = 192;
 ENGINE_F_ENGINE_GET_PREV                    = 116;
 ENGINE_F_ENGINE_INIT                        = 119;
 ENGINE_F_ENGINE_LIST_ADD                    = 120;
 ENGINE_F_ENGINE_LIST_REMOVE                 = 121;
 ENGINE_F_ENGINE_LOAD_PRIVATE_KEY            = 150;
 ENGINE_F_ENGINE_LOAD_PUBLIC_KEY             = 151;
 ENGINE_F_ENGINE_LOAD_SSL_CLIENT_CERT        = 194;
 ENGINE_F_ENGINE_NEW                         = 122;
 ENGINE_F_ENGINE_REMOVE                      = 123;
 ENGINE_F_ENGINE_SET_DEFAULT_STRING          = 189;
 ENGINE_F_ENGINE_SET_DEFAULT_TYPE            = 126;
 ENGINE_F_ENGINE_SET_ID                      = 129;
 ENGINE_F_ENGINE_SET_NAME                    = 130;
 ENGINE_F_ENGINE_TABLE_REGISTER              = 184;
 ENGINE_F_ENGINE_UNLOAD_KEY                  = 152;
 ENGINE_F_ENGINE_UNLOCKED_FINISH             = 191;
 ENGINE_F_ENGINE_UP_REF                      = 190;
 ENGINE_F_INT_CTRL_HELPER                    = 172;
 ENGINE_F_INT_ENGINE_CONFIGURE               = 188;
 ENGINE_F_INT_ENGINE_MODULE_INIT             = 187;
 ENGINE_F_LOG_MESSAGE                        = 141;
 ENGINE_R_ALREADY_LOADED                     = 100;
 ENGINE_R_ARGUMENT_IS_NOT_A_NUMBER           = 133;
 ENGINE_R_CMD_NOT_EXECUTABLE                 = 134;
 ENGINE_R_COMMAND_TAKES_INPUT                = 135;
 ENGINE_R_COMMAND_TAKES_NO_INPUT             = 136;
 ENGINE_R_CONFLICTING_ENGINE_ID              = 103;
 ENGINE_R_CTRL_COMMAND_NOT_IMPLEMENTED       = 119;
 ENGINE_R_DH_NOT_IMPLEMENTED                 = 139;
 ENGINE_R_DSA_NOT_IMPLEMENTED                = 140;
 ENGINE_R_DSO_FAILURE                        = 104;
 ENGINE_R_DSO_NOT_FOUND                      = 132;
 ENGINE_R_ENGINES_SECTION_ERROR              = 148;
 ENGINE_R_ENGINE_CONFIGURATION_ERROR         = 102;
 ENGINE_R_ENGINE_IS_NOT_IN_LIST              = 105;
 ENGINE_R_ENGINE_SECTION_ERROR               = 149;
 ENGINE_R_FAILED_LOADING_PRIVATE_KEY         = 128;
 ENGINE_R_FAILED_LOADING_PUBLIC_KEY          = 129;
 ENGINE_R_FINISH_FAILED                      = 106;
 ENGINE_R_GET_HANDLE_FAILED                  = 107;
 ENGINE_R_ID_OR_NAME_MISSING                 = 108;
 ENGINE_R_INIT_FAILED                        = 109;
 ENGINE_R_INTERNAL_LIST_ERROR                = 110;
 ENGINE_R_INVALID_ARGUMENT                   = 143;
 ENGINE_R_INVALID_CMD_NAME                   = 137;
 ENGINE_R_INVALID_CMD_NUMBER                 = 138;
 ENGINE_R_INVALID_INIT_VALUE                 = 151;
 ENGINE_R_INVALID_STRING                     = 150;
 ENGINE_R_NOT_INITIALISED                    = 117;
 ENGINE_R_NOT_LOADED                         = 112;
 ENGINE_R_NO_CONTROL_FUNCTION                = 120;
 ENGINE_R_NO_INDEX                           = 144;
 ENGINE_R_NO_LOAD_FUNCTION                   = 125;
 ENGINE_R_NO_REFERENCE                       = 130;
 ENGINE_R_NO_SUCH_ENGINE                     = 116;
 ENGINE_R_NO_UNLOAD_FUNCTION                 = 126;
 ENGINE_R_PROVIDE_PARAMETERS                 = 113;
 ENGINE_R_RSA_NOT_IMPLEMENTED                = 141;
 ENGINE_R_UNIMPLEMENTED_CIPHER               = 146;
 ENGINE_R_UNIMPLEMENTED_DIGEST               = 147;
 ENGINE_R_UNIMPLEMENTED_PUBLIC_KEY_METHOD    = 101;
 ENGINE_R_VERSION_INCOMPATIBILITY            = 145;

  ENGINE_CMD_FLAG_NUMERIC       = $0001;
  ENGINE_CMD_FLAG_STRING      = $0002;
  ENGINE_CMD_FLAG_NO_INPUT  = $0004;
  ENGINE_CMD_FLAG_INTERNAL  = $0008;
  ENGINE_CTRL_SET_LOGSTREAM     = 1;
  ENGINE_CTRL_SET_PASSWORD_CALLBACK   = 2;
  ENGINE_CTRL_HUP                             = 3; // Close and reinitialise any  handles/connections etc.
  ENGINE_CTRL_SET_USER_INTERFACE      = 4; { Alternative to callback }
  ENGINE_CTRL_SET_CALLBACK_DATA       = 5; { User-specific data, used   when calling the password
                                                         callback and the user
                                                         interface }
  ENGINE_CTRL_LOAD_CONFIGURATION      = 6; { Load a configuration, given
                                                         a string that represents a
                                                         file name or so }
  ENGINE_CTRL_LOAD_SECTION              = 7; { Load data from a given
                                                         section in the already loaded
                                             configuration }
  ENGINE_CTRL_HAS_CTRL_FUNCTION         = 10;
{ Returns a positive command number for the first command supported by the
 * engine. Returns zero if no ctrl commands are supported. }
  ENGINE_CTRL_GET_FIRST_CMD_TYPE          = 11;
{ The 'long' argument specifies a command implemented by the engine, and the
 * return value is the next command supported, or zero if there are no more. }
  ENGINE_CTRL_GET_NEXT_CMD_TYPE         = 12;
{ The 'void*' argument is a command name (cast from 'const char *'), and the
 * return value is the command that corresponds to it. }
  ENGINE_CTRL_GET_CMD_FROM_NAME         = 13;
{ The next two allow a command to be converted into its corresponding string
 * form. In each case, the 'long' argument supplies the command. In the NAME_LEN
 * case, the return value is the length of the command name (not counting a
 * trailing EOL). In the NAME case, the 'void*' argument must be a string buffer
 * large enough, and it will be populated with the name of the command (WITH a
 * trailing EOL). }
  ENGINE_CTRL_GET_NAME_LEN_FROM_CMD   = 14;
  ENGINE_CTRL_GET_NAME_FROM_CMD         = 15;
{ The next two are similar but give a "short description" of a command. }
  ENGINE_CTRL_GET_DESC_LEN_FROM_CMD   = 16;
  ENGINE_CTRL_GET_DESC_FROM_CMD          = 17;
{ With this command, the return value is the OR'd combination of
 * ENGINE_CMD_FLAG_*** values that indicate what kind of input a given
 * engine-specific ctrl command expects. }
  ENGINE_CTRL_GET_CMD_FLAGS             = 18;

{ ENGINE implementations should start the numbering of their own control
 * commands from this value. (ie. ENGINE_CMD_BASE, ENGINE_CMD_BASE + 1, etc). }
  ENGINE_CMD_BASE                             = 200;
{ NB: These 2 nCipher "chil" control commands are deprecated, and their
 * functionality is now available through ENGINE-specific control commands
 * (exposed through the above-mentioned 'CMD'-handling). Code using these 2
 * commands should be migrated to the more general command handling before these
 * are removed. }
{ Flags specific to the nCipher "chil" engine }
  ENGINE_CTRL_CHIL_SET_FORKCHECK          = 100;
    { Depending on the value of the (long)i argument, this sets or
     * unsets the SimpleForkCheck flag in the CHIL API to enable or
     * disable checking and workarounds for applications that fork().
     }
  ENGINE_CTRL_CHIL_NO_LOCKING             = 101;

  ENGINE_METHOD_RSA               = $0001;
  ENGINE_METHOD_DSA               = $0002;
  ENGINE_METHOD_DH                = $0004;
  ENGINE_METHOD_RAND                = $0008;
  ENGINE_METHOD_ECDH                = $0010;
  ENGINE_METHOD_ECDSA               = $0020;
  ENGINE_METHOD_CIPHERS           = $0040;
  ENGINE_METHOD_DIGESTS           = $0080;
  ENGINE_METHOD_STORE               = $0100;
  ENGINE_METHOD_PKEY_METHS      = $0200;
  ENGINE_METHOD_PKEY_ASN1_METHS = $0400;
  ENGINE_METHOD_ALL               = $FFFF;
  ENGINE_METHOD_NONE                = $0000;


{$ENDREGION}
{$REGION 'RAND'}
const
 RAND_F_RAND_GET_RAND_METHOD                 = 101;
 RAND_F_RAND_INIT_FIPS                       = 102;
 RAND_F_SSLEAY_RAND_BYTES                    = 100;
 RAND_R_ERROR_INITIALISING_DRBG              = 102;
 RAND_R_ERROR_INSTANTIATING_DRBG             = 103;
 RAND_R_NO_FIPS_RANDOM_METHOD_SET            = 101;
 RAND_R_PRNG_NOT_SEEDED                      = 100;
{$ENDREGION}
{$REGION 'CAMELLIA'}
const
 CAMELLIA_BLOCK_SIZE                         = 16;
 CAMELLIA_TABLE_BYTE_LEN                     = 272;
 CAMELLIA_TABLE_WORD_LEN                     = (CAMELLIA_TABLE_BYTE_LEN div 4);
{$ENDREGION}
{$REGION 'DES'}
{$ENDREGION}
{$REGION 'EVP'}
const
  EVP_PKEY_NONE                              = NID_undef;
  EVP_PKEY_RSA                               = NID_rsaEncryption;
  EVP_PKEY_RSA2                              = NID_rsa;
  EVP_PKEY_DSA                               = NID_dsa;
  EVP_PKEY_DSA1                              = NID_dsa_2;
  EVP_PKEY_DSA2                              = NID_dsaWithSHA;
  EVP_PKEY_DSA3                              = NID_dsaWithSHA1;
  EVP_PKEY_DSA4                              = NID_dsaWithSHA1_2;
  EVP_PKEY_DH                                = NID_dhKeyAgreement;
  EVP_PKEY_EC                                = NID_X9_62_id_ecPublicKey;
  EVP_PKEY_HMAC                              = NID_hmac;
  EVP_PKEY_CMAC                              = NID_cmac;
  EVP_PKEY_MO_SIGN                           = $1;
  EVP_PKEY_MO_VERIFY                         = $2;
  EVP_PKEY_MO_ENCRYPT                        = $4;
  EVP_PKEY_MO_DECRYPT                        = $8;
  EVP_PKEY_OP_UNDEFINED                      = 0;
  EVP_PKEY_OP_PARAMGEN                       = 1 shl 1;
  EVP_PKEY_OP_KEYGEN                         = 1 shl 2;
  EVP_PKEY_OP_SIGN                           = 1 shl 3;
  EVP_PKEY_OP_VERIFY                         = 1 shl 4;
  EVP_PKEY_OP_VERIFYRECOVER                  = 1 shl 5;
  EVP_PKEY_OP_SIGNCTX                        = 1 shl 6;
  EVP_PKEY_OP_VERIFYCTX                      = 1 shl 7;
  EVP_PKEY_OP_ENCRYPT                        = 1 shl 8;
  EVP_PKEY_OP_DECRYPT                        = 1 shl 9;
  EVP_PKEY_OP_DERIVE                         = 1 shl 10;

    EVP_CIPH_STREAM_CIPHER          = $0;
    EVP_CIPH_ECB_MODE               = $1;
    EVP_CIPH_CBC_MODE               = $2;
    EVP_CIPH_CFB_MODE               = $3;
    EVP_CIPH_OFB_MODE               = $4;
    EVP_CIPH_CTR_MODE               = $5;
    EVP_CIPH_GCM_MODE               = $6;
    EVP_CIPH_CCM_MODE               = $7;
    EVP_CIPH_XTS_MODE               = $10001;
    EVP_CIPH_MODE                   = $F0007;
    EVP_CIPH_VARIABLE_LENGTH        = $8;
    EVP_CIPH_CUSTOM_IV              = $10;
    EVP_CIPH_ALWAYS_CALL_INIT       = $20;
    EVP_CIPH_CTRL_INIT              = $40;
    EVP_CIPH_CUSTOM_KEY_LENGTH      = $80;
    EVP_CIPH_NO_PADDING             = $100;
    EVP_CIPH_RAND_KEY               = $200;
    EVP_CIPH_CUSTOM_COPY            = $400;
    EVP_CIPH_FLAG_DEFAULT_ASN1      = $1000;
    EVP_CIPH_FLAG_LENGTH_BITS       = $2000;
    EVP_CIPH_FLAG_FIPS              = $4000;
    EVP_CIPH_FLAG_NON_FIPS_ALLOW    = $8000;
    EVP_CIPH_FLAG_CUSTOM_CIPHER     = $100000;
    EVP_CIPH_FLAG_AEAD_CIPHER       = $200000;

    EVP_CTRL_INIT                       = $0;
    EVP_CTRL_SET_KEY_LENGTH             = $1;
    EVP_CTRL_GET_RC2_KEY_BITS           = $2;
    EVP_CTRL_SET_RC2_KEY_BITS           = $3;
    EVP_CTRL_GET_RC5_ROUNDS             = $4;
    EVP_CTRL_SET_RC5_ROUNDS             = $5;
    EVP_CTRL_RAND_KEY                   = $6;
    EVP_CTRL_PBE_PRF_NID                = $7;
    EVP_CTRL_COPY                       = $8;
    EVP_CTRL_GCM_SET_IVLEN              = $9;
    EVP_CTRL_GCM_GET_TAG                = $10;
    EVP_CTRL_GCM_SET_TAG                = $11;
    EVP_CTRL_GCM_SET_IV_FIXED           = $12;
    EVP_CTRL_GCM_IV_GEN                 = $13;
    EVP_CTRL_CCM_SET_IVLEN              = EVP_CTRL_GCM_SET_IVLEN;
    EVP_CTRL_CCM_GET_TAG                = EVP_CTRL_GCM_GET_TAG;
    EVP_CTRL_CCM_SET_TAG                = EVP_CTRL_GCM_SET_TAG;
    EVP_CTRL_CCM_SET_L                  = $14;
    EVP_CTRL_CCM_SET_MSGLEN             = $15;
    EVP_CTRL_AEAD_TLS1_AAD              = $16;
    EVP_CTRL_AEAD_SET_MAC_KEY           = $17;
    EVP_CTRL_GCM_SET_IV_INV             = $18;
    EVP_GCM_TLS_FIXED_IV_LEN            = 4;
    EVP_GCM_TLS_EXPLICIT_IV_LEN         = 8;
    EVP_GCM_TLS_TAG_LEN                 = 16;


  EVP_PKEY_CTRL_MD              = 1;
  EVP_PKEY_CTRL_PEER_KEY        = 2;
  EVP_PKEY_CTRL_PKCS7_ENCRYPT   = 3;
  EVP_PKEY_CTRL_PKCS7_DECRYPT   = 4;
  EVP_PKEY_CTRL_PKCS7_SIGN      = 5;
  EVP_PKEY_CTRL_SET_MAC_KEY     = 6;
  EVP_PKEY_CTRL_DIGESTINIT      = 7;
  EVP_PKEY_CTRL_SET_IV          = 8;
  EVP_PKEY_CTRL_CMS_ENCRYPT     = 9;
  EVP_PKEY_CTRL_CMS_DECRYPT     = 10;
  EVP_PKEY_CTRL_CMS_SIGN        = 11;
  EVP_PKEY_CTRL_CIPHER          = 12;
  EVP_PKEY_ALG_CTRL             = $1000;
  EVP_PKEY_FLAG_AUTOARGLEN      = 2;
  EVP_PKEY_FLAG_SIGCTX_CUSTOM   = 4;

  EVP_PKEY_CTRL_RSA_PADDING     = (EVP_PKEY_ALG_CTRL + 1);
  EVP_PKEY_CTRL_RSA_PSS_SALTLEN  = (EVP_PKEY_ALG_CTRL + 2);

  EVP_PKEY_CTRL_RSA_KEYGEN_BITS  = (EVP_PKEY_ALG_CTRL + 3);
  EVP_PKEY_CTRL_RSA_KEYGEN_PUBEXP    = (EVP_PKEY_ALG_CTRL + 4);
  EVP_PKEY_CTRL_RSA_MGF1_MD  = (EVP_PKEY_ALG_CTRL + 5);

  EVP_PKEY_CTRL_GET_RSA_PADDING      = (EVP_PKEY_ALG_CTRL + 6);
  EVP_PKEY_CTRL_GET_RSA_PSS_SALTLEN  = (EVP_PKEY_ALG_CTRL + 7);
  EVP_PKEY_CTRL_GET_RSA_MGF1_MD      = (EVP_PKEY_ALG_CTRL + 8);


  EVP_PKEY_CTRL_EC_PARAMGEN_CURVE_NID        = (EVP_PKEY_ALG_CTRL + 1);

  EVP_PKEY_OP_TYPE_SIG   = (EVP_PKEY_OP_SIGN  or  EVP_PKEY_OP_VERIFY  or  EVP_PKEY_OP_VERIFYRECOVER  or  EVP_PKEY_OP_SIGNCTX  or  EVP_PKEY_OP_VERIFYCTX);
  EVP_PKEY_OP_TYPE_CRYPT =  (EVP_PKEY_OP_ENCRYPT  or  EVP_PKEY_OP_DECRYPT);
//  EVP_PKEY_OP_TYPE_NOGEN =  (EVP_PKEY_OP_SIG  or  EVP_PKEY_OP_CRYPT  or  EVP_PKEY_OP_DERIVE);
  EVP_PKEY_OP_TYPE_GEN  = (EVP_PKEY_OP_PARAMGEN  or  EVP_PKEY_OP_KEYGEN);



{$ENDREGION}
{$REGION 'ERR'}
const
  ERR_NUM_ERRORS = 16;
{$ENDREGION}
{$REGION 'X509'}
 X509_F_ADD_CERT_DIR                         = 100;
 X509_F_BY_FILE_CTRL                         = 101;
 X509_F_CHECK_POLICY                         = 145;
 X509_F_DIR_CTRL                             = 102;
 X509_F_GET_CERT_BY_SUBJECT                  = 103;
 X509_F_NETSCAPE_SPKI_B64_DECODE             = 129;
 X509_F_NETSCAPE_SPKI_B64_ENCODE             = 130;
 X509_F_X509AT_ADD1_ATTR                     = 135;
 X509_F_X509V3_ADD_EXT                       = 104;
 X509_F_X509_ATTRIBUTE_CREATE_BY_NID         = 136;
 X509_F_X509_ATTRIBUTE_CREATE_BY_OBJ         = 137;
 X509_F_X509_ATTRIBUTE_CREATE_BY_TXT         = 140;
 X509_F_X509_ATTRIBUTE_GET0_DATA             = 139;
 X509_F_X509_ATTRIBUTE_SET1_DATA             = 138;
 X509_F_X509_CHECK_PRIVATE_KEY               = 128;
 X509_F_X509_CRL_PRINT_FP                    = 147;
 X509_F_X509_EXTENSION_CREATE_BY_NID         = 108;
 X509_F_X509_EXTENSION_CREATE_BY_OBJ         = 109;
 X509_F_X509_GET_PUBKEY_PARAMETERS           = 110;
 X509_F_X509_LOAD_CERT_CRL_FILE              = 132;
 X509_F_X509_LOAD_CERT_FILE                  = 111;
 X509_F_X509_LOAD_CRL_FILE                   = 112;
 X509_F_X509_NAME_ADD_ENTRY                  = 113;
 X509_F_X509_NAME_ENTRY_CREATE_BY_NID        = 114;
 X509_F_X509_NAME_ENTRY_CREATE_BY_TXT        = 131;
 X509_F_X509_NAME_ENTRY_SET_OBJECT           = 115;
 X509_F_X509_NAME_ONELINE                    = 116;
 X509_F_X509_NAME_PRINT                      = 117;
 X509_F_X509_PRINT_EX_FP                     = 118;
 X509_F_X509_PUBKEY_GET                      = 119;
 X509_F_X509_PUBKEY_SET                      = 120;
 X509_F_X509_REQ_CHECK_PRIVATE_KEY           = 144;
 X509_F_X509_REQ_PRINT_EX                    = 121;
 X509_F_X509_REQ_PRINT_FP                    = 122;
 X509_F_X509_REQ_TO_X509                     = 123;
 X509_F_X509_STORE_ADD_CERT                  = 124;
 X509_F_X509_STORE_ADD_CRL                   = 125;
 X509_F_X509_STORE_CTX_GET1_ISSUER           = 146;
 X509_F_X509_STORE_CTX_INIT                  = 143;
 X509_F_X509_STORE_CTX_NEW                   = 142;
 X509_F_X509_STORE_CTX_PURPOSE_INHERIT       = 134;
 X509_F_X509_TO_X509_REQ                     = 126;
 X509_F_X509_TRUST_ADD                       = 133;
 X509_F_X509_TRUST_SET                       = 141;
 X509_F_X509_VERIFY_CERT                     = 127;
 X509_R_BAD_X509_FILETYPE                    = 100;
 X509_R_BASE64_DECODE_ERROR                  = 118;
 X509_R_CANT_CHECK_DH_KEY                    = 114;
 X509_R_CERT_ALREADY_IN_HASH_TABLE           = 101;
 X509_R_ERR_ASN1_LIB                         = 102;
 X509_R_INVALID_DIRECTORY                    = 113;
 X509_R_INVALID_FIELD_NAME                   = 119;
 X509_R_INVALID_TRUST                        = 123;
 X509_R_KEY_TYPE_MISMATCH                    = 115;
 X509_R_KEY_VALUES_MISMATCH                  = 116;
 X509_R_LOADING_CERT_DIR                     = 103;
 X509_R_LOADING_DEFAULTS                     = 104;
 X509_R_METHOD_NOT_SUPPORTED                 = 124;
 X509_R_NO_CERT_SET_FOR_US_TO_VERIFY         = 105;
 X509_R_PUBLIC_KEY_DECODE_ERROR              = 125;
 X509_R_PUBLIC_KEY_ENCODE_ERROR              = 126;
 X509_R_SHOULD_RETRY                         = 106;
 X509_R_UNABLE_TO_FIND_PARAMETERS_IN_CHAIN   = 107;
 X509_R_UNABLE_TO_GET_CERTS_PUBLIC_KEY       = 108;
 X509_R_UNKNOWN_KEY_TYPE                     = 117;
 X509_R_UNKNOWN_NID                          = 109;
 X509_R_UNKNOWN_PURPOSE_ID                   = 121;
 X509_R_UNKNOWN_TRUST_ID                     = 120;
 X509_R_UNSUPPORTED_ALGORITHM                = 111;
 X509_R_WRONG_LOOKUP_TYPE                    = 112;
 X509_R_WRONG_TYPE                           = 122;

 X509V3_F_A2I_GENERAL_NAME                       = 164;
 X509V3_F_ASIDENTIFIERCHOICE_CANONIZE            = 161;
 X509V3_F_ASIDENTIFIERCHOICE_IS_CANONICAL        = 162;
 X509V3_F_COPY_EMAIL                             = 122;
 X509V3_F_COPY_ISSUER                            = 123;
 X509V3_F_DO_DIRNAME                             = 144;
 X509V3_F_DO_EXT_CONF                            = 124;
 X509V3_F_DO_EXT_I2D                             = 135;
 X509V3_F_DO_EXT_NCONF                           = 151;
 X509V3_F_DO_I2V_NAME_CONSTRAINTS                = 148;
 X509V3_F_GNAMES_FROM_SECTNAME                   = 156;
 X509V3_F_HEX_TO_STRING                          = 111;
 X509V3_F_I2S_ASN1_ENUMERATED                    = 121;
 X509V3_F_I2S_ASN1_IA5STRING                     = 149;
 X509V3_F_I2S_ASN1_INTEGER                       = 120;
 X509V3_F_I2V_AUTHORITY_INFO_ACCESS              = 138;
 X509V3_F_NOTICE_SECTION                         = 132;
 X509V3_F_NREF_NOS                               = 133;
 X509V3_F_POLICY_SECTION                         = 131;
 X509V3_F_PROCESS_PCI_VALUE                      = 150;
 X509V3_F_R2I_CERTPOL                            = 130;
 X509V3_F_R2I_PCI                                = 155;
 X509V3_F_S2I_ASN1_IA5STRING                     = 100;
 X509V3_F_S2I_ASN1_INTEGER                       = 108;
 X509V3_F_S2I_ASN1_OCTET_STRING                  = 112;
 X509V3_F_S2I_ASN1_SKEY_ID                       = 114;
 X509V3_F_S2I_SKEY_ID                            = 115;
 X509V3_F_SET_DIST_POINT_NAME                    = 158;
 X509V3_F_STRING_TO_HEX                          = 113;
 X509V3_F_SXNET_ADD_ID_ASC                       = 125;
 X509V3_F_SXNET_ADD_ID_INTEGER                   = 126;
 X509V3_F_SXNET_ADD_ID_ULONG                     = 127;
 X509V3_F_SXNET_GET_ID_ASC                       = 128;
 X509V3_F_SXNET_GET_ID_ULONG                     = 129;
 X509V3_F_V2I_ASIDENTIFIERS                      = 163;
 X509V3_F_V2I_ASN1_BIT_STRING                    = 101;
 X509V3_F_V2I_AUTHORITY_INFO_ACCESS              = 139;
 X509V3_F_V2I_AUTHORITY_KEYID                    = 119;
 X509V3_F_V2I_BASIC_CONSTRAINTS                  = 102;
 X509V3_F_V2I_CRLD                               = 134;
 X509V3_F_V2I_EXTENDED_KEY_USAGE                 = 103;
 X509V3_F_V2I_GENERAL_NAMES                      = 118;
 X509V3_F_V2I_GENERAL_NAME_EX                    = 117;
 X509V3_F_V2I_IDP                                = 157;
 X509V3_F_V2I_IPADDRBLOCKS                       = 159;
 X509V3_F_V2I_ISSUER_ALT                         = 153;
 X509V3_F_V2I_NAME_CONSTRAINTS                   = 147;
 X509V3_F_V2I_POLICY_CONSTRAINTS                 = 146;
 X509V3_F_V2I_POLICY_MAPPINGS                    = 145;
 X509V3_F_V2I_SUBJECT_ALT                        = 154;
 X509V3_F_V3_ADDR_VALIDATE_PATH_INTERNAL         = 160;
 X509V3_F_V3_GENERIC_EXTENSION                   = 116;
 X509V3_F_X509V3_ADD1_I2D                        = 140;
 X509V3_F_X509V3_ADD_VALUE                       = 105;
 X509V3_F_X509V3_EXT_ADD                         = 104;
 X509V3_F_X509V3_EXT_ADD_ALIAS                   = 106;
 X509V3_F_X509V3_EXT_CONF                        = 107;
 X509V3_F_X509V3_EXT_I2D                         = 136;
 X509V3_F_X509V3_EXT_NCONF                       = 152;
 X509V3_F_X509V3_GET_SECTION                     = 142;
 X509V3_F_X509V3_GET_STRING                      = 143;
 X509V3_F_X509V3_GET_VALUE_BOOL                  = 110;
 X509V3_F_X509V3_PARSE_LIST                      = 109;
 X509V3_F_X509_PURPOSE_ADD                       = 137;
 X509V3_F_X509_PURPOSE_SET                       = 141;


 X509V3_R_BAD_IP_ADDRESS                                = 118;
 X509V3_R_BAD_OBJECT                                    = 119;
 X509V3_R_BN_DEC2BN_ERROR                               = 100;
 X509V3_R_BN_TO_ASN1_INTEGER_ERROR                      = 101;
 X509V3_R_DIRNAME_ERROR                                 = 149;
 X509V3_R_DISTPOINT_ALREADY_SET                         = 160;
 X509V3_R_DUPLICATE_ZONE_ID                             = 133;
 X509V3_R_ERROR_CONVERTING_ZONE                         = 131;
 X509V3_R_ERROR_CREATING_EXTENSION                      = 144;
 X509V3_R_ERROR_IN_EXTENSION                            = 128;
 X509V3_R_EXPECTED_A_SECTION_NAME                       = 137;
 X509V3_R_EXTENSION_EXISTS                              = 145;
 X509V3_R_EXTENSION_NAME_ERROR                          = 115;
 X509V3_R_EXTENSION_NOT_FOUND                           = 102;
 X509V3_R_EXTENSION_SETTING_NOT_SUPPORTED               = 103;
 X509V3_R_EXTENSION_VALUE_ERROR                         = 116;
 X509V3_R_ILLEGAL_EMPTY_EXTENSION                       = 151;
 X509V3_R_ILLEGAL_HEX_DIGIT                             = 113;
 X509V3_R_INCORRECT_POLICY_SYNTAX_TAG                   = 152;
 X509V3_R_INVALID_MULTIPLE_RDNS                         = 161;
 X509V3_R_INVALID_ASNUMBER                              = 162;
 X509V3_R_INVALID_ASRANGE                               = 163;
 X509V3_R_INVALID_BOOLEAN_STRING                        = 104;
 X509V3_R_INVALID_EXTENSION_STRING                      = 105;
 X509V3_R_INVALID_INHERITANCE                           = 165;
 X509V3_R_INVALID_IPADDRESS                             = 166;
 X509V3_R_INVALID_NAME                                  = 106;
 X509V3_R_INVALID_NULL_ARGUMENT                         = 107;
 X509V3_R_INVALID_NULL_NAME                             = 108;
 X509V3_R_INVALID_NULL_VALUE                            = 109;
 X509V3_R_INVALID_NUMBER                                = 140;
 X509V3_R_INVALID_NUMBERS                               = 141;
 X509V3_R_INVALID_OBJECT_IDENTIFIER                     = 110;
 X509V3_R_INVALID_OPTION                                = 138;
 X509V3_R_INVALID_POLICY_IDENTIFIER                     = 134;
 X509V3_R_INVALID_PROXY_POLICY_SETTING                  = 153;
 X509V3_R_INVALID_PURPOSE                               = 146;
 X509V3_R_INVALID_SAFI                                  = 164;
 X509V3_R_INVALID_SECTION                               = 135;
 X509V3_R_INVALID_SYNTAX                                = 143;
 X509V3_R_ISSUER_DECODE_ERROR                           = 126;
 X509V3_R_MISSING_VALUE                                 = 124;
 X509V3_R_NEED_ORGANIZATION_AND_NUMBERS                 = 142;
 X509V3_R_NO_CONFIG_DATABASE                            = 136;
 X509V3_R_NO_ISSUER_CERTIFICATE                         = 121;
 X509V3_R_NO_ISSUER_DETAILS                             = 127;
 X509V3_R_NO_POLICY_IDENTIFIER                          = 139;
 X509V3_R_NO_PROXY_CERT_POLICY_LANGUAGE_DEFINED         = 154;
 X509V3_R_NO_PUBLIC_KEY                                 = 114;
 X509V3_R_NO_SUBJECT_DETAILS                            = 125;
 X509V3_R_ODD_NUMBER_OF_DIGITS                          = 112;
 X509V3_R_OPERATION_NOT_DEFINED                         = 148;
 X509V3_R_OTHERNAME_ERROR                               = 147;
 X509V3_R_POLICY_LANGUAGE_ALREADY_DEFINED               = 155;
 X509V3_R_POLICY_PATH_LENGTH                            = 156;
 X509V3_R_POLICY_PATH_LENGTH_ALREADY_DEFINED            = 157;
 X509V3_R_POLICY_SYNTAX_NOT_CURRENTLY_SUPPORTED         = 158;
 X509V3_R_POLICY_WHEN_PROXY_LANGUAGE_REQUIRES_NO_POLICY = 159;
 X509V3_R_SECTION_NOT_FOUND                             = 150;
 X509V3_R_UNABLE_TO_GET_ISSUER_DETAILS                  = 122;
 X509V3_R_UNABLE_TO_GET_ISSUER_KEYID                    = 123;
 X509V3_R_UNKNOWN_BIT_STRING_ARGUMENT                   = 111;
 X509V3_R_UNKNOWN_EXTENSION                             = 129;
 X509V3_R_UNKNOWN_EXTENSION_NAME                        = 130;
 X509V3_R_UNKNOWN_OPTION                                = 120;
 X509V3_R_UNSUPPORTED_OPTION                            = 117;
 X509V3_R_UNSUPPORTED_TYPE                              = 167;
 X509V3_R_USER_TOO_LONG                                 = 132;

 X509V3_EXT_DYNAMIC     = $1;
 X509V3_EXT_CTX_DEP     = $2;
 X509V3_EXT_MULTILINE   = $4;

 GEN_OTHERNAME  = 0;
 GEN_EMAIL      = 1;
 GEN_DNS        = 2;
 GEN_X400       = 3;
 GEN_DIRNAME    = 4;
 GEN_EDIPARTY   = 5;
 GEN_URI        = 6;
 GEN_IPADD      = 7;
 GEN_RID        = 8;

 CRLDP_ALL_REASONS                  = $807f;
 CRL_REASON_NONE                    = -1;
 CRL_REASON_UNSPECIFIED             = 0;
 CRL_REASON_KEY_COMPROMISE          = 1;
 CRL_REASON_CA_COMPROMISE           = 2;
 CRL_REASON_AFFILIATION_CHANGED     = 3;
 CRL_REASON_SUPERSEDED              = 4;
 CRL_REASON_CESSATION_OF_OPERATION  = 5;
 CRL_REASON_CERTIFICATE_HOLD        = 6;
 CRL_REASON_REMOVE_FROM_CRL         = 8;
 CRL_REASON_PRIVILEGE_WITHDRAWN     = 9;
 CRL_REASON_AA_COMPROMISE           = 10;

 IDP_PRESENT        = $1;
 IDP_INVALID        = $2;
 IDP_ONLYUSER       = $4;
 IDP_ONLYCA         = $8;
 IDP_ONLYATTR       = $10;
 IDP_INDIRECT       = $20;
 IDP_REASONS        = $40;

 EXFLAG_BCONS           = $1;
 EXFLAG_KUSAGE          = $2;
 EXFLAG_XKUSAGE         = $4;
 EXFLAG_NSCERT          = $8;
 EXFLAG_CA              = $10;
 EXFLAG_SI              = $20;
 EXFLAG_SS              = $20;
 EXFLAG_V1              = $40;
 EXFLAG_INVALID         = $80;
 EXFLAG_SET             = $100;
 EXFLAG_CRITICAL        = $200;
 EXFLAG_PROXY           = $400;

 EXFLAG_INVALID_POLICY  = $800;
 EXFLAG_FRESHEST        = $1000;

 KU_DIGITAL_SIGNATURE   = $0080;
 KU_NON_REPUDIATION     = $0040;
 KU_KEY_ENCIPHERMENT    = $0020;
 KU_DATA_ENCIPHERMENT   = $0010;
 KU_KEY_AGREEMENT       = $0008;
 KU_KEY_CERT_SIGN       = $0004;
 KU_CRL_SIGN            = $0002;
 KU_ENCIPHER_ONLY       = $0001;
 KU_DECIPHER_ONLY       = $8000;

 NS_SSL_CLIENT      = $80;
 NS_SSL_SERVER      = $40;
 NS_SMIME           = $20;
 NS_OBJSIGN         = $10;
 NS_SSL_CA          = $04;
 NS_SMIME_CA        = $02;
 NS_OBJSIGN_CA      = $01;
 NS_ANY_CA          = (NS_SSL_CA or NS_SMIME_CA or NS_OBJSIGN_CA);

 XKU_SSL_SERVER     = $1    ;
 XKU_SSL_CLIENT     = $2;
 XKU_SMIME          = $4;
 XKU_CODE_SIGN      = $8;
 XKU_SGC            = $10;
 XKU_OCSP_SIGN      = $20;
 XKU_TIMESTAMP      = $40;
 XKU_DVCS           = $80;

 X509_PURPOSE_DYNAMIC       = $1;
 X509_PURPOSE_DYNAMIC_NAME  = $2;

 X509_PURPOSE_SSL_CLIENT        = 1;
 X509_PURPOSE_SSL_SERVER        = 2;
 X509_PURPOSE_NS_SSL_SERVER     = 3;
 X509_PURPOSE_SMIME_SIGN        = 4;
 X509_PURPOSE_SMIME_ENCRYPT     = 5;
 X509_PURPOSE_CRL_SIGN          = 6;
 X509_PURPOSE_ANY               = 7;
 X509_PURPOSE_OCSP_HELPER       = 8;
 X509_PURPOSE_TIMESTAMP_SIGN    = 9;

 X509_PURPOSE_MIN       = 1;
 X509_PURPOSE_MAX       = 9;

 X509V3_EXT_UNKNOWN_MASK        = ($f shl 16);
 X509V3_EXT_DEFAULT             = 0;
 X509V3_EXT_ERROR_UNKNOWN       = (1 shl 16);
 X509V3_EXT_PARSE_UNKNOWN       = (2 shl 16);
 X509V3_EXT_DUMP_UNKNOWN        = (3 shl 16);

 X509V3_ADD_OP_MASK             = $f;
 X509V3_ADD_DEFAULT             = 0;
 X509V3_ADD_APPEND              = 1;
 X509V3_ADD_REPLACE             = 2;
 X509V3_ADD_REPLACE_EXISTING    = 3;
 X509V3_ADD_KEEP_EXISTING       = 4;
 X509V3_ADD_DELETE              = 5;
 X509V3_ADD_SILENT              = $10;

{$ENDREGION}

{$REGION 'PKCS7}

 PKCS7_TEXT             = $1;
 PKCS7_NOCERTS          = $2;
 PKCS7_NOSIGS           = $4;
 PKCS7_NOCHAIN          = $8;
 PKCS7_NOINTERN         = $10;
 PKCS7_NOVERIFY         = $20;
 PKCS7_DETACHED         = $40;
 PKCS7_BINARY           = $80;
 PKCS7_NOATTR           = $100;
 PKCS7_NOSMIMECAP       = $200;
 PKCS7_NOOLDMIMETYPE    = $400;
 PKCS7_CRLFEOL          = $800;
 PKCS7_STREAM           = $1000;
 PKCS7_NOCRL            = $2000;
 PKCS7_PARTIAL          = $4000;
 PKCS7_REUSE_DIGEST     = $8000;

 SMIME_TEXT     = PKCS7_TEXT;
 SMIME_NOCERTS  = PKCS7_NOCERTS;
 SMIME_NOSIGS   = PKCS7_NOSIGS;
 SMIME_NOCHAIN  = PKCS7_NOCHAIN;
 SMIME_NOINTERN = PKCS7_NOINTERN;
 SMIME_NOVERIFY = PKCS7_NOVERIFY;
 SMIME_DETACHED = PKCS7_DETACHED;
 SMIME_BINARY   = PKCS7_BINARY;
 SMIME_NOATTR   = PKCS7_NOATTR;


{$ENDREGION}

const
  CRYPTO_LOCK       = 1;
  CRYPTO_UNLOCK     = 2;
  CRYPTO_READ       = 4;
  CRYPTO_WRITE   =  8;

const
  HMAC_MAX_MD_CBLOCK     = 128;

    MD4_CBLOCK      = 64;
    MD4_LBLOCK      = (MD4_CBLOCK div 4);
    MD4_DIGEST_LENGTH  = 16;

    MD5_CBLOCK      = 64;
    MD5_LBLOCK      = (MD5_CBLOCK div 4);
    MD5_DIGEST_LENGTH  = 16;

    MDC2_BLOCK              = 8;
    MDC2_DIGEST_LENGTH      = 16;

    RC5_32_BLOCK        = 8;
    RC5_32_KEY_LENGTH   = 16;

    RC5_8_ROUNDS        = 8;
    RC5_12_ROUNDS       = 12;
    RC5_16_ROUNDS       = 16;

    RIPEMD160_CBLOCK    = 64;
    RIPEMD160_LBLOCK    = (RIPEMD160_CBLOCK div 4);
    RIPEMD160_DIGEST_LENGTH     = 20;

    PKCS5_SALT_LEN			= 8;

  SSL_MAX_SSL_SESSION_ID_LENGTH		= 32;
  SSL_MAX_SID_CTX_LENGTH			    = 32;

  SSL_MAX_KRB5_PRINCIPAL_LENGTH   = 256;
  SSL_MIN_RSA_MODULUS_LENGTH_IN_BYTES	  = (512 div 8);
  SSL_MAX_KEY_ARG_LENGTH			= 8;
  SSL_MAX_MASTER_KEY_LENGTH		= 48;

type
    qword = UInt64;
    PVOID = Pointer;
    TC_INT   = LongInt;
    PC_INT   = ^TC_INT;
    TC_UINT  = LongWord;
    PC_UINT  = ^TC_UINT;
    TC_LONG  = LongInt;
    PC_LONG = ^TC_LONG;
    TC_ULONG = LongWord;
    PC_ULONG = ^TC_ULONG;
    TC_ULONGLONG = qword;
    TC_time_t = TC_LONG;
    TC_USHORT = Word;
    PC_USHORT = ^TC_USHORT;
    TC_SIZE_T = LongWord;
    PC_SIZE_T = ^TC_SIZE_T;
    BN_ULONG = TC_ULONGLONG;
    PBN_ULONG = ^BN_ULONG;
    TC_UCHAR = AnsiChar;
    DES_LONG= TC_ULONG;
    PDES_LONG = ^DES_LONG;
    point_conversion_form_t = byte;
    TC_OSSL_SSIZE_T = TC_LONG;
    IDEA_INT = TC_UINT;
    MD4_LONG = TC_ULONG;
    MD5_LONG = TC_ULONG;
    RC2_INT = TC_UINT;
    RC4_INT = TC_UINT;
    RC5_32_INT = TC_ULONG;
    RIPEMD160_LONG = TC_ULONG;
    SHA_LONG = TC_ULONG;
    SHA_LONG64 = UInt64;

    // AMP: Needed in Delphi 2010
    size_t = Word;

type

  PX509_PUBKEY = ^X509_PUBKEY;
  PPKCS8_PRIV_KEY_INFO = ^PKCS8_PRIV_KEY_INFO;
  PPPKCS8_PRIV_KEY_INFO = ^PPKCS8_PRIV_KEY_INFO;

  PX509_ALGOR  = ^X509_ALGOR;
  PPX509_ALGOR =^PX509_ALGOR;
  PEVP_MD_CTX = ^EVP_MD_CTX;
  PPEVP_MD_CTX = ^PEVP_MD_CTX;

  PENGINE = ^ENGINE;
  PPENGINE = ^PENGINE;
  PRSA_METHOD = ^RSA_METHOD;
  PUI_METHOD = Pointer;
  PSSL = ^SSL;
  PEVP_PKEY = ^EVP_PKEY;
  PX509 = ^X509;
  PEVP_MD = ^EVP_MD;
  PPEVP_MD = ^PEVP_MD;
  PEVP_CIPHER = ^EVP_CIPHER;
  PPEVP_CIPHER = ^PEVP_CIPHER;
  PEVP_PKEY_METHOD = ^EVP_PKEY_METHOD;
  PPEVP_PKEY_METHOD = ^PEVP_PKEY_METHOD;
  PEVP_PKEY_ASN1_METHOD = ^EVP_PKEY_ASN1_METHOD;
  PPEVP_PKEY_ASN1_METHOD = ^PEVP_PKEY_ASN1_METHOD;


  STACK = record
    num : TC_INT;
    data : PPAnsiChar;
    sorted : TC_INT;
    num_alloc : TC_INT;
    comp : function (_para1: PPAnsiChar; _para2: PPAnsiChar):  TC_INT; cdecl;
  end;

  STACK_OF = record
    _stack: STACK;
  end;
  PSTACK_OF = ^STACK_OF;
  PSTACK    = PSTACK_OF;
  PSTACK_OF_X509 = PSTACK_OF;
  PSTACK_OF_X509_NAME = PSTACK_OF;

  CRYPTO_EX_DATA = record
    sk : PSTACK;
    dummy : TC_INT;
  end;

  PENGINE_CMD_DEFN = ^ENGINE_CMD_DEFN;
  ENGINE_CMD_DEFN = record
    cmd_num: TC_UINT;
    cmd_name: PAnsiChar;
    cmd_desc: PAnsiChar;
    cmd_flags: TC_UINT;
  end;

  ENGINE_CB_FUNC = procedure;
  ENGINE_GEN_FUNC_PTR = function: TC_INT; cdecl;
  ENGINE_GEN_INT_FUNC_PTR = function(engine: PENGINE): TC_INT; cdecl;
  ENGINE_CTRL_FUNC_PTR = function(engine: PENGINE; _par1: TC_INT; _par2: TC_LONG; _par3: Pointer; f: ENGINE_CB_FUNC): TC_INT; cdecl;
  ENGINE_LOAD_KEY_PTR = function(engine: PENGINE; buf: PAnsiChar; ui_method: PUI_METHOD; callback_data: Pointer): PEVP_PKEY; cdecl;
  ENGINE_SSL_CLIENT_CERT_PTR = function(engine: PENGINE; ssl: PSSL; ca_dn: PSTACK_OF_X509_NAME; var cert: PX509; var key: PEVP_PKEY; var pother: PSTACK_OF_X509; ui_method: PUI_METHOD; callback_data: Pointer): TC_INT; cdecl;
  ENGINE_CIPHERS_PTR =  function(engine: PENGINE; cipher: PPEVP_CIPHER; var par1: PC_INT; par2: TC_INT): TC_INT; cdecl;
  ENGINE_DIGESTS_PTR = function(engine: PENGINE; md: PPEVP_MD; var par1: PC_INT; par2: TC_INT): TC_INT; cdecl;
  ENGINE_PKEY_METHS_PTR = function(engine: PENGINE; meth: PPEVP_PKEY_METHOD; var par1: PC_INT; par2: TC_INT): TC_INT; cdecl;
  ENGINE_PKEY_ASN1_METHS_PTR = function(engine: PENGINE; meth: PPEVP_PKEY_ASN1_METHOD; var par1: PC_INT; par2: TC_INT): TC_INT; cdecl;
  ENGINE = record
    id: PAnsiChar;
    name: PAnsiChar;
    rsa_meth: PRSA_METHOD;
    dsa_meth: Pointer;
    dh_meth: Pointer;
    ecdh_meth: Pointer;
    ecdsa_meth: Pointer;
    rand_meth: Pointer;
    store_meth: Pointer;
    ciphers: ENGINE_CIPHERS_PTR;
    digests: ENGINE_DIGESTS_PTR;
    pkey_meths: ENGINE_PKEY_METHS_PTR;
    pkey_asn1_meths: ENGINE_PKEY_ASN1_METHS_PTR;
    _destroy: ENGINE_GEN_INT_FUNC_PTR;
    init: ENGINE_GEN_INT_FUNC_PTR;
    finish: ENGINE_GEN_INT_FUNC_PTR;
    ctrl: ENGINE_CTRL_FUNC_PTR;
    load_privkey: ENGINE_LOAD_KEY_PTR;
    load_pubkey: ENGINE_LOAD_KEY_PTR;
    load_ssl_client_cert: ENGINE_SSL_CLIENT_CERT_PTR;
    cmd_defns: PENGINE_CMD_DEFN;
    flags: TC_INT;
    struct_ref: TC_INT;
    funct_ref: TC_INT;
    ex_data: CRYPTO_EX_DATA;
    prev: PENGINE;
    next: PENGINE;
  end;

  BF_LONG = TC_ULONG;
  PBF_LONG = ^BF_LONG;

  BUF_MEM = record
    length : TC_SIZE_T;
    data : PAnsiChar;
    max: TC_SIZE_T;
  end;
  PBUF_MEM = ^BUF_MEM;

  POBJ_NAME = ^OBJ_NAME;
  OBJ_NAME = record
      _type: TC_INT;
    _alias: TC_INT;
      _name: PAnsiChar;
      _data: PAnsiChar;
  end;
  OBJ_NAME_CALLBACK = procedure(_par1: POBJ_NAME; arg: Pointer); cdecl;
  OBJ_CMP_CALLBACK = function(_par1, _par2: Pointer): TC_INT; cdecl;

  OBJ_hash_func = function(_par1: PAnsiChar): TC_ULONG; cdecl;
  OBJ_cmp_func = function(_par1: PAnsiChar; _par2: PAnsiChar): TC_INT; cdecl;
  OBJ_free_func = procedure(_par1: PAnsiChar; _par2: TC_INT; _par3: PAnsiChar); cdecl;

{$REGION 'CRYPTO'}

  PCRYPTO_THREADID = ^CRYPTO_THREADID;

  CRYPTO_THREADID = record
    ptr: Pointer;
    val: TC_ULONG;
  end;

  PCRYPTO_EX_DATA = ^CRYPTO_EX_DATA;

  CRYPTO_EX_new = function(parent: Pointer; ptr: Pointer; ad: PCRYPTO_EX_DATA;
                    idx: TC_INT; argl: TC_LONG; argp: Pointer): TC_INT; cdecl;

  CRYPTO_EX_free = procedure(parent: Pointer; ptr: Pointer; ad: PCRYPTO_EX_DATA;
                    idx: TC_INT; argl: TC_LONG; argp: Pointer); cdecl;

  CRYPTO_EX_dup = function(_to: PCRYPTO_EX_DATA; _from: PCRYPTO_EX_DATA; from_d: Pointer;
                    idx: TC_INT; argl: TC_LONG; argp: Pointer): TC_INT; cdecl;

  CRYPTO_mem_alloc_func = function(_size: TC_SIZE_T): Pointer; cdecl;
  CRYPTO_mem_realloc_func = function(_mem: Pointer; _size: TC_SIZE_T): Pointer; cdecl;
  CRYPTO_mem_free_func = procedure(_mem: pointer); cdecl;

{$ENDREGION}


{$REGION 'ERR'}

  ERR_STATE = record
    tid: CRYPTO_THREADID;
    err_flags: array [0..ERR_NUM_ERRORS-1] of TC_INT;
    err_buffer: array[0..ERR_NUM_ERRORS-1] of TC_ULONG;
    err_data: array[0..ERR_NUM_ERRORS-1] of PAnsiChar;
    err_data_flags: array[0..ERR_NUM_ERRORS-1] of TC_INT;
    err_file: array[0..ERR_NUM_ERRORS-1] of PAnsiChar;
    err_line: array[0..ERR_NUM_ERRORS-1] of TC_INT;
    top,bottom: TC_INT;
  end;
  PERR_STATE = ^ERR_STATE;

  ERR_STRING_DATA = record
      error: TC_LONG;
      _string: PAnsiChar;
  end;
  PERR_STRING_DATA = ^ERR_STRING_DATA;

  ERR_CALLBACK = function(str: PAnsiChar; len: TC_SIZE_T; u: Pointer): TC_INT; cdecl;
  PERR_FNS = Pointer;

{$ENDREGION}


{$REGION 'BN'}
  BIGNUM = record
    d : PBN_ULONG;
    top : TC_INT;
    dmax : TC_INT;
    neg : TC_INT;
    flags : TC_INT;
  end;
  PBIGNUM = ^BIGNUM;
  PPBIGNUM = ^PBIGNUM;
  BIGNUM_ARR = array[0..1] of BIGNUM;
  PBIGNUM_ARR = ^BIGNUM_ARR;

  PBN_GENCB = ^BN_GENCB;
  BN_cb_1 = procedure (p1, p2 : TC_INT; p3 : Pointer); cdecl;
  BN_cb_2 = function (p1, p2 : TC_INT; p3 : PBN_GENCB): TC_INT; cdecl;

  BN_GENCB_union = record
    case Integer of
        0 : (cb_1 : BN_cb_1);
        1 : (cb_2 : BN_cb_2);
  end;

  BN_GENCB = record
    ver : TC_UINT;
    arg : Pointer;
    cb : BN_GENCB_union;
  end;

  BN_MONT_CTX = record
    ri : TC_INT;
    RR: BIGNUM;
    N: BIGNUM;
    Ni: BIGNUM;
    n0 : array[0..1] of  BN_ULONG;
    flags : TC_INT;
  end;
  PBN_MONT_CTX = ^BN_MONT_CTX;
  PPBN_MONT_CTX = ^PBN_MONT_CTX;

  PBN_CTX_STACK = ^BN_CTX_STACK;
  BN_CTX_STACK = record
    indexes: PC_UINT;
    depth: TC_UINT;
    size: TC_UINT;
  end;
  BN_STACK = BN_CTX_STACK;
  PBN_STACK = ^BN_STACK;

  PBN_POOL_ITEM = ^BN_POOL_ITEM;
  BN_POOL_ITEM = record
    vals: array[0..BN_CTX_POOL_SIZE-1] of BIGNUM;
    prev: PBN_POOL_ITEM;
    next: PBN_POOL_ITEM;
  end;

  BN_POOL = record
    head: PBN_POOL_ITEM;
    current: PBN_POOL_ITEM;
    tail: PBN_POOL_ITEM;
    used: TC_UINT;
    size: TC_UINT;
  end;

  PBN_CTX = ^BN_CTX;
  BN_CTX = record
   pool: BN_POOL;
   stack: BN_STACK;
   used: TC_UINT;
   err_stack: TC_INT;
   too_many: TC_INT;
  end;

  PPBN_CTX = ^PBN_CTX;
  PBN_BLINDING = ^BN_BLINDING;

  BN_BLINDING = record
   A: PBIGNUM;
   Ai: PBIGNUM;
   e: PBIGNUM;
   _mod: PBIGNUM;
   thread_id: TC_ULONG;
   tid: CRYPTO_THREADID;
   counter: TC_INT;
   flags: TC_ULONG;
   m_ctx: PBN_MONT_CTX;
   bn_mod_exp: function(r: PBIGNUM; a: PBIGNUM; p: PBIGNUM; m: PBIGNUM; ctx: PBN_CTX; m_ctx: PBN_MONT_CTX): TC_INT;
  end;

  PBN_RECP_CTX = ^BN_RECP_CTX;
  BN_RECP_CTX = record
   N: BIGNUM;
   Nr: BIGNUM;
   num_bits: TC_INT;
   shift: TC_INT;
   flags: TC_INT;
  end;

  BN_mod_exp_cb = function(r: PBIGNUM; a: PBIGNUM; p: PBIGNUM; m: PBIGNUM; ctx: PBN_CTX; m_ctx: PBN_MONT_CTX): TC_INT; cdecl;
{$ENDREGION}

{$REGION 'CAST'}


  CAST_LONG = TC_UINT;
  PCAST_LONG = ^CAST_LONG;

  CAST_KEY = record
    data: array[0..31] of CAST_LONG;
    short_key: TC_INT;
  end;
  PCAST_KEY = ^CAST_KEY;

{$ENDREGION}

{$REGION 'EC'}
  EC_builtin_curve = record
    nid : TC_INT;
    comment : PAnsiChar;
  end;

  PEC_GROUP = ^EC_GROUP;
  PPEC_GROUP = ^PEC_GROUP;

  PEC_METHOD = ^EC_METHOD;

  PEC_POINT = ^EC_POINT;
  EC_POINT = record
    meth: PEC_METHOD;
    X: BIGNUM;
    Y: BIGNUM;
    Z: BIGNUM;
    Z_is_one: TC_INT;
  end;
  EC_POINT_ARR = array[0..0] of EC_POINT;
  PEC_POINT_ARR = ^EC_POINT_ARR;

  EC_METHOD = record
    flags: TC_INT;
    field_type: TC_INT;
    group_init: function: PEC_GROUP; cdecl;
    group_finsh: procedure(_group: PEC_GROUP); cdecl;
    group_clear_finish: procedure(_group: PEC_GROUP); cdecl;
    group_copy: function(dst: PEC_GROUP; src: PEC_GROUP): TC_INT; cdecl;
    group_set_curve: function(_gr: PEC_GROUP; p: PBIGNUM; a: PBIGNUM; b: PBIGNUM; ctx: PBN_CTX): TC_INT; cdecl;
    group_get_curve: function(_gr: PEC_GROUP; p: PBIGNUM; a: PBIGNUM; b: PBIGNUM; ctx: PBN_CTX): TC_INT; cdecl;
    group_get_degree: function(g: PEC_GROUP): TC_INT; cdecl;
    group_check_discriminant: function(_gr: PEC_GROUP; ctx: PBN_CTX): TC_INT; cdecl;
    point_init: function: PEC_POINT;
    point_finish: procedure(p: PEC_POINT); cdecl;
    point_finish_clear: procedure(p: PEC_POINT); cdecl;
    point_copy: function(p1, p2: PEC_POINT): TC_INT; cdecl;
    point_set_to_infinity: function(g: PEC_GROUP; p: PEC_POINT): TC_INT; cdecl;
    point_set_Jprojective_coordinates_GFp: function(g: PEC_GROUP; p: PEC_POINT; x,y,z: PBIGNUM; ctx: PBN_CTX): TC_INT; cdecl;
    point_get_Jprojective_coordinates_GFp: function(g: PEC_GROUP; p: PEC_POINT; x,y,z: PBIGNUM; ctx: PBN_CTX): TC_INT; cdecl;
    point_set_affine_coordinates: function(g: PEC_GROUP; p: PEC_POINT; x,y,z: PBIGNUM; ctx: PBN_CTX): TC_INT; cdecl;
    point_get_affine_coordinates: function(g: PEC_GROUP; p: PEC_POINT; x,y,z: PBIGNUM; ctx: PBN_CTX): TC_INT; cdecl;
    point_set_compressed_coordinates: function(g: PEC_GROUP; p: PEC_POINT; x: PBIGNUM; y_bit: TC_INT; ctx: PBN_CTX): TC_INT; cdecl;
    point2oct: function(g: PEC_GROUP; p: PEC_POINT; _form: point_conversion_form_t; buf: PAnsiChar; len: TC_SIZE_T; ctx: PBN_CTX): TC_SIZE_T; cdecl;
    oct2point: function(g: PEC_GROUP; p: PEC_POINT; buf: PAnsiChar; len: TC_SIZE_T; ctx: PBN_CTX): TC_INT; cdecl;
    add: function(g: PEC_GROUP; r,a,b: PEC_POINT; ctx: PBN_CTX): TC_INT; cdecl;
    dbl: function(g: PEC_GROUP; r,a: PEC_POINT; ctx: PBN_CTX): TC_INT; cdecl;
    invert: function(g: PEC_GROUP; p: PEC_POINT; ctx: PBN_CTX): TC_INT; cdecl;
    is_at_infinity: function(g: PEC_GROUP; p: PEC_POINT): TC_INT; cdecl;
    is_on_curve: function(g: PEC_GROUP; p: PEC_POINT; ctx: PBN_CTX): TC_INT; cdecl;
    point_cmp: function(g: PEC_GROUP; a,b: PEC_POINT; ctx: PBN_CTX): TC_INT; cdecl;
    make_affine: function(g: PEC_GROUP; p: PEC_POINT; ctx: PBN_CTX): TC_INT; cdecl;
    points_make_affine: function(g: PEC_GROUP; num: TC_SIZE_T; p: PEC_POINT_ARR; ctx: PBN_CTX): TC_INT; cdecl;
    mul: function(g: PEC_GROUP; r: PEC_POINT; scalar: PBIGNUM; num: TC_SIZE_T; points: PEC_POINT_ARR; scalars: PBIGNUM_ARR; ctx: PBN_CTX): TC_INT; cdecl;
    precompute_mult: function(g: PEC_GROUP; ctx: PBN_CTX): TC_INT; cdecl;
    have_precompute_mult: function(g: PEC_GROUP): TC_INT; cdecl;
      field_mul: function(g: PEC_GROUP; r, a, b: PBIGNUM; ctx: PBN_CTX): TC_INT; cdecl;
      field_sqr: function(g: PEC_GROUP; r, a: PBIGNUM; ctx: PBN_CTX): TC_INT; cdecl;
      field_div: function(g: PEC_GROUP; r, a, b: PBIGNUM; ctx: PBN_CTX): TC_INT; cdecl;
      field_encode: function(g: PEC_GROUP; r, a: PBIGNUM; ctx: PBN_CTX): TC_INT; cdecl;
      field_decode: function(g: PEC_GROUP; r, a: PBIGNUM; ctx: PBN_CTX): TC_INT; cdecl;
      field_set_to_one: function(g: PEC_GROUP; r : PBIGNUM; ctx: PBN_CTX): TC_INT; cdecl;
  end;

  PEC_EXTRA_DATA = ^EC_EXTRA_DATA;
  PPEC_EXTRA_DATA = ^PEC_EXTRA_DATA;
  EC_EXTRA_DATA = record
    next: PEC_EXTRA_DATA;
    data: Pointer;
      dup_func: function: pointer; cdecl;
      free_func: procedure(_par: Pointer); cdecl;
      clear_free_func: procedure(_par: Pointer); cdecl;
  end;


  EC_GROUP = record
    meth: PEC_METHOD;
    generator: PEC_POINT;
    order, cofactor: BIGNUM;
    curve_name : TC_INT;
    asn1_flag: TC_INT;
    asn1_form: point_conversion_form_t;
    seed: PAnsiChar;
    seed_len: TC_SIZE_T;
    extra_data: PEC_EXTRA_DATA;
    field : BIGNUM;
    poly: array[0..5] of TC_INT;
    a, b: BIGNUM;
    a_is_minus3: TC_INT;
    field_data1: Pointer;
    field_data2: Pointer;
    field_mod_func: function(a, b, c: PBIGNUM; ctx: PBN_CTX): TC_INT; cdecl;
  end;

  EC_builtin_curves = array[0..0] of EC_builtin_curve;
  PEC_builtin_curves = ^EC_builtin_curves;


  PEC_KEY = ^EC_KEY;
  PPEC_KEY = ^PEC_KEY;
  EC_KEY = record
    version: TC_INT;
    group: PEC_GROUP;

    pub_key: PEC_POINT;
    priv_key:PBIGNUM;

    enc_flag: TC_UINT;
    conv_form: point_conversion_form_t;

    references: TC_INT;
    flags: TC_INT;
    method_data: PEC_EXTRA_DATA;
  end;

  PEC_PKEY_CTX = ^EC_PKEY_CTX;
  PPEC_PKEY_CTX = ^PEC_PKEY_CTX;
  EC_PKEY_CTX = record
      gen_group: PEC_GROUP;
      md: PEVP_MD;
  end;

  EC_dup_func = function(par: Pointer): Pointer; cdecl;
  EC_free_func = procedure(par: Pointer); cdecl;
  EC_clear_free_func = procedure(par: Pointer); cdecl;


{$ENDREGION}


  PSTACK_OF_IPAddressFamily = PSTACK;
  PSTACK_OF_ASN1_TYPE = PSTACK; // may be ^
  PSTACK_OF_ASN1_OBJECT = PSTACK;
  PSTACK_OF_ASN1_INTEGER = PSTACK_OF;
  PSTACK_OF_GENERAL_NAME = PSTACK;
  PGENERAL_NAMES = PSTACK_OF_GENERAL_NAME;
  PPGENERAL_NAMES = ^PGENERAL_NAMES;
  PSTACK_OF_GENERAL_NAMES = PSTACK_OF;
  PPSTACK_OF_GENERAL_NAMES = ^PSTACK_OF_GENERAL_NAMES;
  PSTACK_OF_ASIdOrRange = PSTACK;
  PASIdOrRanges = PSTACK_OF_ASIdOrRange;
  PSTACK_OF_CONF_VALUE = PSTACK;
  PPSTACK_OF_CONF_VALUE = ^PSTACK_OF_CONF_VALUE;



  BIT_STRING_BITNAME = record
    bitnum : TC_INT;
    lname : PAnsiChar;
    sname : PAnsiChar;
  end;
  PBIT_STRING_BITNAME = ^BIT_STRING_BITNAME;


{$REGION 'BIO'}
  PBIO = ^BIO;
  PPBIO = ^PBIO;
  PBIO_METHOD = ^BIO_METHOD;

  Pbio_info_cb = procedure (_para1 : PBIO; _para2 : TC_INT; _para3 : PAnsiChar;
     _para4 : TC_INT; _para5, _para6 : TC_LONG); cdecl;
  pbio_dump_cb = function(data: Pointer; len: TC_SIZE_T; u: pointer): TC_INT; cdecl;

  BIO_METHOD = record
    _type : TC_INT;
    name : PAnsiChar;
    bwrite : function(_para1 : PBIO; _para2 : PAnsiChar; _para3 : TC_Int) : TC_Int; cdecl;
    bread : function(_para1: PBIO; _para2: PAnsiChar; _para3: TC_Int) : TC_Int; cdecl;
    bputs : function (_para1 : PBIO; _para2 : PAnsiChar) : TC_Int; cdecl;
    bgets : function (_para1 : PBIO; _para2 : PAnsiChar; _para3 : TC_Int) : TC_Int; cdecl;
    ctrl : function (_para1 : PBIO; _para2 : TC_Int; _para3 : TC_LONG; _para4 : Pointer) : TC_LONG; cdecl;
    create : function(_para1 : PBIO) : TC_Int; cdecl;
    destroy : function (_para1 : PBIO) : TC_Int; cdecl;
    callback_ctrl : function (_para1 : PBIO; _para2 : TC_Int; _para3 : pbio_info_cb): TC_LONG; cdecl;
  end;

  BIO = record
    method : PBIO_METHOD;
    callback : function (_para1 : PBIO; _para2 : TC_INT; _para3 : PAnsiChar;
       _para4 : TC_INT; _para5, _para6 : TC_LONG) : TC_LONG cdecl;
    cb_arg : PAnsiChar;
    init : TC_INT;
    shutdown : TC_INT;
    flags : TC_INT;
    retry_reason : TC_INT;
    num : TC_INT;
    ptr : Pointer;
    next_bio : PBIO;
    prev_bio : PBIO;
    references : TC_INT;
    num_read : TC_ULONG;
    num_write : TC_ULONG;
    ex_data : CRYPTO_EX_DATA;
  end;

{$ENDREGION}

{$REGION 'ASN1'}
  ASN1_OBJECT = record
    sn, ln : PAnsiChar;
    nid    : TC_INT;
    length : TC_INT;
    data   : PAnsiChar;
    flags  : TC_INT; // Should we free this one
  end;

  asn1_string_st = record
    length : TC_INT;
    _type : TC_INT;
    data : PAnsiChar;
    flags : TC_LONG;
  end;

  ASN1_STRING = asn1_string_st;
  PASN1_STRING = ^ASN1_STRING;
  PPASN1_STRING = ^PASN1_STRING;
  ASN1_INTEGER = ASN1_STRING;
  PASN1_INTEGER = ^ASN1_INTEGER;
  PPASN1_INTEGER = ^PASN1_INTEGER;

  PASN1_OBJECT = ^ASN1_OBJECT;
  PPASN1_OBJECT = ^PASN1_OBJECT;

  ASN1_UNIVERSALSTRING = ASN1_STRING;
  PASN1_UNIVERSALSTRING = ^ASN1_UNIVERSALSTRING;
  PPASN1_UNIVERSALSTRING = ^PASN1_UNIVERSALSTRING;
  ASN1_BMPSTRING = ASN1_STRING;
  PASN1_BMPSTRING = ^ASN1_BMPSTRING;
  PPASN1_BMPSTRING = ^PASN1_BMPSTRING;
  ASN1_VISIBLESTRING = ASN1_STRING;
  PASN1_VISIBLESTRING = ^ASN1_VISIBLESTRING;
  PPASN1_VISIBLESTRING = ^PASN1_VISIBLESTRING;
  ASN1_UTF8STRING = ASN1_STRING;
  PASN1_UTF8STRING = ^ASN1_UTF8STRING;
  PPASN1_UTF8STRING = ^PASN1_UTF8STRING;
  ASN1_BOOLEAN = TC_INT;
  PASN1_BOOLEAN = ^ASN1_BOOLEAN;
  PPASN1_BOOLEAN = ^PASN1_BOOLEAN;
  ASN1_NULL = TC_INT;
  PASN1_NULL = ^ASN1_NULL;
  PPASN1_NULL = ^PASN1_NULL;
  ASN1_ENUMERATED = ASN1_STRING;
  PASN1_ENUMERATED = ^ASN1_ENUMERATED;
  PPASN1_ENUMERATED = ^PASN1_ENUMERATED;
  ASN1_BIT_STRING = ASN1_STRING;
  PASN1_BIT_STRING = ^ASN1_BIT_STRING;
  PPASN1_BIT_STRING = ^PASN1_BIT_STRING;
  ASN1_OCTET_STRING = ASN1_STRING;
  PASN1_OCTET_STRING = ^ASN1_OCTET_STRING;
  PPASN1_OCTET_STRING = ^PASN1_OCTET_STRING;
  ASN1_PRINTABLESTRING = ASN1_STRING;
  PASN1_PRINTABLESTRING = ^ASN1_PRINTABLESTRING;
  PPASN1_PRINTABLESTRING = ^PASN1_PRINTABLESTRING;
  ASN1_T61STRING = ASN1_STRING;
  PASN1_T61STRING = ^ASN1_T61STRING;
  PPASN1_T61STRING = ^PASN1_T61STRING;
  ASN1_IA5STRING = ASN1_STRING;
  PASN1_IA5STRING = ^ASN1_IA5STRING;
  PPASN1_IA5STRING = ^PASN1_IA5STRING;
  ASN1_GENERALSTRING = ASN1_STRING;
  PASN1_GENERALSTRING = ^ASN1_GENERALSTRING;
  PPASN1_GENERALSTRING = ^PASN1_GENERALSTRING;
  ASN1_UTCTIME = ASN1_STRING;
  PASN1_UTCTIME = ^ASN1_UTCTIME;
  PPASN1_UTCTIME = ^PASN1_UTCTIME;
  ASN1_GENERALIZEDTIME = ASN1_STRING;
  PASN1_GENERALIZEDTIME = ^ASN1_GENERALIZEDTIME;
  PPASN1_GENERALIZEDTIME = ^PASN1_GENERALIZEDTIME;
  ASN1_TIME = ASN1_STRING;
  PASN1_TIME = ^ASN1_TIME;
  PPASN1_TIME = ^PASN1_TIME;

  ASN1_ENCODING = record
    enc: PAnsiChar;
    len: TC_LONG;
    modified: TC_INT;
  end;
  PASN1_ENCODING = ^ASN1_ENCODING;

  ASN1_TYPE = record
    case Integer of
      0:  (ptr: PAnsiChar);
      1:  (boolean: ASN1_BOOLEAN);
      2:  (asn1_string: PASN1_STRING);
      3:  (_object: PASN1_OBJECT);
      4:  (integer: PASN1_INTEGER);
      5:  (enumerated: PASN1_ENUMERATED);
      6:  (bit_string: PASN1_BIT_STRING);
      7:  (octet_string: PASN1_OCTET_STRING);
      8:  (printablestring: PASN1_PRINTABLESTRING);
      9:  (t61string: PASN1_T61STRING);
      10: (ia5string: PASN1_IA5STRING);
      11: (generalstring: PASN1_GENERALSTRING);
      12: (bmpstring: PASN1_BMPSTRING);
      13: (universalstring: PASN1_UNIVERSALSTRING);
      14: (utctime: PASN1_UTCTIME);
      15: (generalizedtime: PASN1_GENERALIZEDTIME);
      16: (visiblestring: PASN1_VISIBLESTRING);
      17: (utf8string: PASN1_UTF8STRING);
      18: (_set: PASN1_STRING);
      19: (sequence: PASN1_STRING);
  end;
  PASN1_TYPE = ^ASN1_TYPE;
  PPASN1_TYPE = ^PASN1_TYPE;

  AUTHORITY_KEYID = record
    keyid : PASN1_OCTET_STRING;
    issuer : PGENERAL_NAMES;
    serial : PASN1_INTEGER;
  end;
  PAUTHORITY_KEYID = ^AUTHORITY_KEYID;
  PPAUTHORITY_KEYID = ^PAUTHORITY_KEYID;

  PASRange = ^ASRange;
  ASRange = record
    min, max: PASN1_INTEGER;
  end;

  ASIdOrRange_union = record
  case byte of
    0: (id: PASN1_INTEGER);
    1: (range: PASRange);
  end;

  PASIdOrRange = ^ASIdOrRange;
  ASIdOrRange = record
    _type: TC_INT;
    u: ASIdOrRange_union;
  end;

  ASIdentifierChoice_union = record
  case byte of
   ASIdentifierChoice_inherit : (inherit : PASN1_NULL);
   ASIdentifierChoice_asIdsOrRanges : (asIdsOrRanges : PASIdOrRanges);
  end;
  ASIdentifierChoice = record
    _type : TC_INT;
    u : ASIdentifierChoice_union;
  end;
  PASIdentifierChoice = ^ASIdentifierChoice;


  ASIdentifiers = record
    asnum : PASIdentifierChoice;
    rdi : PASIdentifierChoice;
  end;
  PASIdentifiers = ^ASIdentifiers;

  PIPAddressRange = ^IPAddressRange;
  IPAddressRange = record
    min, max: PASN1_BIT_STRING;
  end;

  IPAddressOrRange_union = record
  case byte of
    0: (addressPrefix: PASN1_BIT_STRING);
    1: (addressRange: PIPAddressRange);
  end;

  PIPAddressOrRange = ^IPAddressOrRange;
  IPAddressOrRange = record
    _type: TC_INT;
    u: IPAddressOrRange_union;
  end;

  PSTACK_OF_IPAddressOrRange = PSTACK_OF;
  PIPAddressOrRanges = PSTACK_OF_IPAddressOrRange;

  IPAddressChoice_union = record
  case byte of
    0: (inherit: PASN1_NULL);
    1: (addressesOrRanges: PIPAddressOrRanges);
  end;

  PIPAddressChoice = ^IPAddressChoice;
  IPAddressChoice = record
    _type: TC_INT;
    u: IPAddressChoice_union;
  end;

  PIPAddressFamily = ^IPAddressFamily;
  IPAddressFamily = record
    addressFamily: PASN1_OCTET_STRING;
    ipAddressChoice: PIPAddressChoice;
  end;

  PIPAddrBlocks = PSTACK_OF_IPAddressFamily;

  ASN1_CTX = record
    p : PAnsiChar;   // work char pointer
    eos : TC_INT;    // end of sequence read for indefinite encoding
    error : TC_INT;  // error code to use when returning an error
    inf : TC_INT;    // constructed if 0x20; indefinite is 0x21
    tag : TC_INT;    // tag from last 'get object'
    xclass : TC_INT; // class from last 'get object'
    slen : TC_LONG;  // length of last 'get object'
    max : PAnsiChar; // largest value of p allowed
    q : PAnsiChar;   // temporary variable
    pp : PPAnsiChar; // variable
    line : TC_INT;   // used in error processing
  end;
  PASN1_CTX = ^ASN1_CTX;

  ASN1_PCTX = record
    flags: TC_ULONG;
    nm_flags: TC_ULONG;
    cert_flags: TC_ULONG;
    oid_flags: TC_ULONG;
    str_flags: TC_ULONG;
  end;

  PASN1_PCTX = ^ASN1_PCTX;

  void_func = function: pointer;
  I2D_OF_void = function(_para1 : Pointer; var _para2 : PByte) : TC_INT cdecl;
  D2I_OF_void = function (_para1 : PPointer;  var _para2 : PByte; _para3 : TC_LONG) : Pointer cdecl;

  ASN1_METHOD = record
    i2d : i2d_of_void;
    d2i : i2d_of_void;
    create : function: Pointer; cdecl;
    destroy : procedure(ptr: Pointer); cdecl;
  end;
  PASN1_METHOD = ^ASN1_METHOD;

  ASN1_HEADER = record
    header : PASN1_OCTET_STRING;
    data : Pointer;
    meth : PASN1_METHOD;
  end;
  PASN1_HEADER = ^ASN1_HEADER;

  ASN1_STRING_TABLE = record
    nid : TC_INT;
    minsize : TC_LONG;
    maxsize : TC_LONG;
    mask : TC_ULONG;
    flags : TC_ULONG;
  end;
  PASN1_STRING_TABLE = ^ASN1_STRING_TABLE;
  PSTACK_OF_ASN1_STRING_TABLE = PSTACK;

  PASN1_ITEM = ^ASN1_ITEM;
  PASN1_ITEM_EXP = PASN1_ITEM;

  ASN1_TEMPLATE = record
    flags : TC_ULONG;         // Various flags
    tag : TC_LONG;            // tag; not used if no tagging
    offset : TC_ULONG;        // Offset of this field in structure
    field_name : PAnsiChar;   // Field name
    item : PASN1_ITEM_EXP;    // Relevant ASN1_ITEM or ASN1_ADB
  end;

  PASN1_TEMPLATE = ^ASN1_TEMPLATE;
  ASN1_ITEM = record
    itype : Char;               // The item type; primitive; SEQUENCE; CHOICE or extern
    utype : TC_LONG;            // underlying type
    templates : PASN1_TEMPLATE; // If SEQUENCE or CHOICE this contains the contents
    tcount : TC_LONG;           // Number of templates if SEQUENCE or CHOICE
    funcs : Pointer;            // functions that handle this type
    size : TC_LONG;             // Structure size (usually)
    sname : PAnsiChar;            // Structure name
  end;

  PSTACK_OF_ASN1_ADB_TABLE = PSTACK;
  PPSTACK_OF_ASN1_ADB_TABLE = ^PSTACK_OF_ASN1_ADB_TABLE;
  PASN1_ADB_TABLE = ^ASN1_ADB_TABLE;
  PASN1_ADB = ^ASN1_ADB;

  ASN1_ADB = record
    flags : TC_ULONG;                       // Various flags
    offset : TC_ULONG;                      // Offset of selector field
    app_items : PPSTACK_OF_ASN1_ADB_TABLE;  // Application defined items
    tbl : PASN1_ADB_TABLE;                  // Table of possible types
    tblcount : TC_LONG;                     // Number of entries in tbl
    default_tt : PASN1_TEMPLATE;            // Type to use if no match
    null_tt : PASN1_TEMPLATE;               // Type to use if selector is NULL
  end;
  ASN1_ADB_TABLE = record
    flags : TC_LONG;                        // Various flags
    offset : TC_LONG;                         // Offset of selector field
    app_items : PPSTACK_OF_ASN1_ADB_TABLE;  // Application defined items
    tbl : PASN1_ADB_TABLE;                  // Table of possible types
    tblcount : TC_LONG;                     // Number of entries in tbl
    default_tt : PASN1_TEMPLATE;            // Type to use if no match
    null_tt : PASN1_TEMPLATE;               // Type to use if selector is NULL
  end;

  PASN1_TLC = ^ASN1_TLC;
  ASN1_TLC = record
      valid : Byte;     // Values below are valid
      ret : TC_INT;     // return value
      plen : TC_LONG;     // length
      ptag : TC_INT;      // class value
      pclass : TC_INT;  // class value
      hdrlen : TC_INT;  // header length
  end;
  PASN1_VALUE = Pointer;
  PPASN1_VALUE = ^PASN1_VALUE;

  ASN1_new_func = function : PASN1_VALUE; cdecl;
  ASN1_free_func = procedure (a : PASN1_VALUE); cdecl;
  ASN1_d2i_func = function (a : PASN1_VALUE; var _in : PByte; length : TC_LONG ) : PASN1_VALUE; cdecl;
  ASN1_i2d_func = function (a : PASN1_VALUE; var _in : PByte)  : TC_INT; cdecl;
  ASN1_ex_d2i = function(var pval : PASN1_VALUE; var _in : PByte; len : TC_LONG; it : PASN1_ITEM; tag, aclass : TC_INT;
                   opt : Byte; ctx : PASN1_TLC) : TC_INT; cdecl;
  ASN1_ex_i2d = function(var pval : PASN1_VALUE; var _out : PByte;  it : PASN1_ITEM; tag, aclass : TC_INT) : TC_INT; cdecl;
  ASN1_ex_new_func = function(var pval : PASN1_VALUE; it : PASN1_ITEM) : TC_INT; cdecl;
  ASN1_ex_free_func = procedure(var pval : PASN1_VALUE; it : PASN1_ITEM); cdecl;
  ASN1_primitive_i2c = function (var pval : PASN1_VALUE; cont : PByte; putype : PC_INT; it : PASN1_ITEM ) : TC_INT; cdecl;
  ASN1_primitive_c2i = function (var pval : PASN1_VALUE; cont : PByte; len, utype : TC_INT; free_cont : PByte; it: PASN1_ITEM) : TC_INT; cdecl;

  ASN1_COMPAT_FUNCS = record
      asn1_new : ASN1_new_func;
      asn1_free : ASN1_free_func;
      asn1_d2i : ASN1_d2i_func;
     asn1_i2d : ASN1_i2d_func;
  end;
  PASN1_COMPAT_FUNCS = ^ASN1_COMPAT_FUNCS;

  ASN1_EXTERN_FUNCS = record
      app_data : Pointer;
    asn1_ex_new : ASN1_ex_new_func;   //    ASN1_ex_new_func *asn1_ex_new;
    asn1_ex_free : ASN1_ex_free_func; //    ASN1_ex_free_func *asn1_ex_free;
    asn1_ex_clear: ASN1_ex_free_func; //    ASN1_ex_free_func *asn1_ex_clear;
    asn1_ex_d2i : ASN1_ex_d2i;        //    ASN1_ex_d2i *asn1_ex_d2i;
    asn1_ex_i2d : ASN1_ex_i2d;        //    ASN1_ex_i2d *asn1_ex_i2d;
  end;
  PASN1_EXTERN_FUNCS = ^ASN1_EXTERN_FUNCS;

  ASN1_PRIMITIVE_FUNCS = record
    app_data : Pointer;
    flags : TC_ULONG;
    prim_new : ASN1_ex_new_func;
    prim_free : ASN1_ex_free_func;
    prim_clear : ASN1_ex_free_func;
    prim_c2i : ASN1_primitive_c2i;
    prim_i2c : ASN1_primitive_i2c;
  end;
  PASN1_PRIMITIVE_FUNCS = ^ASN1_PRIMITIVE_FUNCS;

  ASN1_aux_cb = function (operation : TC_INT; var _in : PASN1_VALUE; it : PASN1_ITEM) : TC_INT; cdecl;
  asn1_ps_func = function(b: PBIO; var pbuf: PAnsiChar; plen: PC_INT; var parg: Pointer): TC_INT; cdecl;

  ASN1_AUX = record
    app_data : Pointer;
    flags : TC_INT;
    ref_offset : TC_INT;        // Offset of reference value
    ref_lock : TC_INT;        // Lock type to use
    asn1_cb : ASN1_aux_cb;
    enc_offset : TC_INT;        // Offset of ASN1_ENCODING structure
  end;
  PASN1_AUX = ^ASN1_AUX;

{$ENDREGION}

{$REGION 'LHASH'}

  PLHASH_NODE = ^LHASH_NODE;
  PPLHASH_NODE = ^PLHASH_NODE;
  LHASH_NODE = record
    data : Pointer;
    next : PLHASH_NODE;
    hash : TC_ULONG;
  end;

  LHASH_COMP_FN_TYPE = function (const p1;p2 : Pointer) : TC_INT; cdecl;
  LHASH_HASH_FN_TYPE = function(const p1 : Pointer) : TC_ULONG; cdecl;
    LHASH_DOALL_FN_TYPE = procedure(_p: pointer); cdecl;
    LHASH_DOALL_ARG_FN_TYPE = procedure(_p1, _p2: Pointer); cdecl;

  LHASH = record
    b : PPLHASH_NODE;
    comp : LHASH_COMP_FN_TYPE;
    hash : LHASH_HASH_FN_TYPE;
    num_nodes : TC_UINT;
    num_alloc_nodes : TC_UINT;
    p : TC_UINT;
    pmax : TC_UINT;
    up_load : TC_ULONG;
    down_load : TC_ULONG;
    num_items : TC_ULONG;
    num_expands : TC_ULONG;
    num_expand_reallocs : TC_ULONG;
    num_contracts : TC_ULONG;
    num_contract_reallocs : TC_ULONG;
    num_hash_calls : TC_ULONG;
    num_comp_calls : TC_ULONG;
    num_insert : TC_ULONG;
    num_replace : TC_ULONG;
    num_delete : TC_ULONG;
    num_no_delete : TC_ULONG;
    num_retrieve : TC_ULONG;
    num_retrieve_miss : TC_ULONG;
    num_hash_comps : TC_ULONG;
    error : TC_INT;
  end;
  PLHASH = ^LHASH;
  _LHASH = LHASH;
  P_LHASH = ^_LHASH;

  PLHASH_OF = ^LHASH_OF;
  PPLHASH_OF = ^PLHASH_OF;
  LHASH_OF = record
    dummy: TC_INT;
  end; { record }

{$ENDREGION}

{$REGION 'CONF'}
  CONF_VALUE = record
    section : PAnsiChar;
    name : PAnsiChar;
    value : PAnsiChar;
  end;
  PCONF_VALUE = ^CONF_VALUE;
  PLHASH_OF_CONF_VALUE = PLHASH;

  PCONF_METHOD = ^CONF_METHOD;
  PCONF_IMODULE = ^CONF_IMODULE;
  PCONF_MODULE = ^CONF_MODULE;
  PCONF = ^CONF;

  CONF_METHOD = record
    name: PAnsiChar;
    create: function(meth: PCONF_METHOD): PCONF; cdecl;
    init: function(_conf: PCONF): TC_INT; cdecl;
    destroy: function(_conf: PCONF): TC_INT; cdecl;
    destroy_data: function(_conf: PCONF): TC_INT; cdecl;
    load_bio: function(_conf: PCONF; bp: PBIO; eline: PC_LONG): TC_INT; cdecl;
    dump: function(_conf: PCONF; bp: PBIO): TC_INT; cdecl;
    is_number: function(_conf: PCONF; c: AnsiChar): TC_INT; cdecl;
    to_int: function(_conf: PCONF; c: AnsiChar): TC_INT; cdecl;
    load: function(_conf: PCONF; name: PAnsiChar; eline: PC_LONG): TC_INT; cdecl;
  end;

  CONF = record
    method: PCONF_METHOD;
    meth_data: Pointer;
    data: PLHASH;
  end;

  t_list_cb = function(const  _elem: PAnsiChar; _len: TC_INT; usr: Pointer): TC_INT; cdecl;
  t_conf_init_func = function(md: PCONF_IMODULE; const cnf: PCONF): TC_INT; cdecl;
  t_conf_finish_func = procedure(md: PCONF_IMODULE); cdecl;

  CONF_MODULE = record
    dso: Pointer;
     _name: PAnsiChar;
    init: t_conf_init_func;
    finish: t_conf_finish_func;
    _links: TC_INT;
    usr_data: Pointer;
  end;

  CONF_IMODULE = record
	pmod: PCONF_MODULE;
	 _name: PAnsiChar;
	 _value: PAnsiChar;
	s: TC_ULONG;
	usr_data: Pointer;
  end;


{$ENDREGION}

{$REGION 'DH'}

  DH_Callback = procedure(_par1: TC_INT;_par2: TC_INT; _par3: Pointer); cdecl;
  PDH = ^DH;
  PPDH = ^PDH;

  PDH_METHOD = ^DH_METHOD;
  DH_METHOD = record
    name: PAnsiChar;
    generate_key: function(_dh: PDH): TC_INT; cdecl;
    compute_key: function(key: PAnsiChar; pub_key: PBIGNUM; _dh: PDH): TC_INT; cdecl;
    bn_mod_exp: function(_dh: PDH; r: PBIGNUM; a: PBIGNUM; p: PBIGNUM; m: PBIGNUM; ctx: PBN_CTX; m_ctx: PBN_MONT_CTX): TC_INT; cdecl;
    init: function(_dh: PDH): TC_INT; cdecl;
    finish: function(_dh: PDH): TC_INT; cdecl;
    flags: TC_INT;
    app_data: PAnsiChar;
    generate_params: function(_dh: PDH; prime_len: TC_INT; generator: TC_INT; cb: PBN_GENCB): TC_INT; cdecl;
  end;

  DH = record
    pad : TC_INT;
    version : TC_INT;
    p: PBIGNUM;
    g: PBIGNUM;
    _length: TC_LONG;
    pub_key: PBIGNUM;
    priv_key: PBIGNUM;
    flags: TC_INT;
    method_mont_p: PBN_MONT_CTX;
    q: PBIGNUM;
    j: PBIGNUM;
    seed: PAnsiChar;
    seedlen: TC_INT;
    counter: PBIGNUM;
    references: TC_INT;
    ex_data: CRYPTO_EX_DATA;
    meth : PDH_METHOD;
    engine: PENGINE;
  end;

{$ENDREGION}


{$REGION 'RSA'}

  PRSA = ^RSA;
  PPRSA = ^PRSA;
  RSA_METHOD = record
    name : PAnsiChar;
    rsa_pub_enc : function (flen : TC_INT; const from : PAnsiChar;
      _to : PAnsiChar; rsa : PRSA; padding : TC_INT) : TC_INT; cdecl;
    rsa_pub_dec : function (flen : TC_INT; const from : PAnsiChar;
      _to : PAnsiChar; rsa : PRSA; padding : TC_INT) : TC_INT; cdecl;
    rsa_priv_enc : function (flen : TC_INT; const from : PAnsiChar;
      _to : PAnsiChar; rsa : PRSA; padding : TC_INT) : TC_INT; cdecl;
    rsa_priv_dec : function (flen : TC_INT; const from : PAnsiChar;
       _to : PAnsiChar; rsa : PRSA; padding : TC_INT) : TC_INT; cdecl;
    rsa_mod_exp : function (r0 : PBIGNUM; const I : PBIGNUM;  rsa : PRSA; ctx : PBN_CTX) : TC_INT cdecl;
    bn_mod_exp : function (r : PBIGNUM; const a : PBIGNUM; const p : PBIGNUM; const m: PBIGNUM; ctx : PBN_CTX;
      m_ctx : PBN_MONT_CTX ) : TC_INT; cdecl;
    init : function (rsa : PRSA) : TC_INT; cdecl;
    finish : function (rsa : PRSA) : TC_INT; cdecl;
    flags : TC_INT;
    app_data : PAnsiChar;
    rsa_sign : function (_type : TC_INT; const m : PAnsiChar; m_length : TC_UINT; sigret : PAnsiChar; siglen : PC_UINT; const rsa : PRSA) : TC_INT; cdecl;
    rsa_verify : function(dtype : TC_INT; const m : PAnsiChar; m_length : PC_UINT; sigbuf : PAnsiChar; siglen : PC_UINT; const rsa :PRSA) : TC_INT; cdecl;
    rsa_keygen : function (rsa : PRSA; bits : TC_INT; e : PBIGNUM; cb : PBN_GENCB) : TC_INT; cdecl;
  end;

  RSA = record
    pad : TC_INT;
    version : TC_LONG;
    meth : PRSA_METHOD;
    engine : PENGINE;
    n : PBIGNUM;
    e : PBIGNUM;
    d : PBIGNUM;
    p : PBIGNUM;
    q : PBIGNUM;
    dmp1 : PBIGNUM;
    dmq1 : PBIGNUM;
    iqmp : PBIGNUM;
    ex_data : CRYPTO_EX_DATA;
    references : TC_INT;
    flags : TC_INT;
    _method_mod_n : PBN_MONT_CTX;
    _method_mod_p : PBN_MONT_CTX;
    _method_mod_q : PBN_MONT_CTX;
    bignum_data : PAnsiChar;
    blinding : PBN_BLINDING;
    mt_blinding : PBN_BLINDING;
  end;

  PRSA_PKEY_CTX = ^RSA_PKEY_CTX;
  RSA_PKEY_CTX = record
      nbits: TC_INT;
      pub_exp: PBIGNUM;
      gentmp: array[0..1] of TC_INT;
      pad_mode: TC_INT;
      md: PEVP_MD;
      mgf1md: PEVP_MD;
      saltlen: TC_INT;
      tbuf: PAnsiChar;
  end;

  PRSA_PSS_PARAMS = ^RSA_PSS_PARAMS;
  PPRSA_PSS_PARAMS = ^PRSA_PSS_PARAMS;

  RSA_PSS_PARAMS = record
    _hashAlgorithm: PX509_ALGOR;
    _maskGenAlgorithm: PX509_ALGOR;
    _saltLength: PASN1_INTEGER;
    _trailerField: PASN1_INTEGER;
  end; { record }

  RSA_NET_CALLBACK_FUNC = function (_buf: PAnsiChar; _len: TC_INT; const _prompt: PAnsiChar; _verify: TC_INT): TC_INT; cdecl;
{$ENDREGION}


{$REGION 'DSA'}
  PDSA = ^DSA;
  PPDSA = ^PDSA;
  DSA_SIG = record
    r : PBIGNUM;
    s : PBIGNUM;
  end;
  PDSA_SIG = ^DSA_SIG;
  PPDSA_SIG = ^PDSA_SIG;

  DSA_METHOD = record
    name : PAnsiChar;
    dsa_do_sign : function (const dgst : PAnsiChar; dlen : TC_INT; dsa : PDSA) : PDSA_SIG; cdecl;
    dsa_sign_setup : function (dsa : PDSA; ctx_in : PBN_CTX; kinvp, rp : PPBN_CTX) : TC_INT; cdecl;
    dsa_do_verify : function(dgst : PAnsiChar; dgst_len : TC_INT; sig : PDSA_SIG; dsa : PDSA) : TC_INT; cdecl;
    dsa_mod_exp : function(dsa : PDSA; rr, a1, p1, a2, p2, m : PBIGNUM; ctx : PBN_CTX; in_mont : PBN_MONT_CTX) : TC_INT; cdecl;
    bn_mod_exp : function (dsa : PDSA; r, a : PBIGNUM; const p; m : PBIGNUM;  ctx : PBN_CTX; m_ctx : PBN_CTX): TC_INT; cdecl; // Can be null
    init : function (dsa : PDSA) : TC_INT; cdecl;
    finish : function (dsa : PDSA) : TC_INT; cdecl;
    flags : TC_INT;
    app_data : PAnsiChar;
    dsa_paramgen : function (dsa : PDSA; bits : TC_INT; seed : PAnsiChar;  seed_len : TC_INT; counter_ret : PC_INT; h_ret : PC_ULONG;
       cb : PBN_GENCB ) : TC_INT; cdecl;
    dsa_keygen : function(dsa : PDSA) : TC_INT; cdecl;
  end;
  PDSA_METHOD = ^DSA_METHOD;

  DSA = record
    pad : TC_INT;
    version : TC_LONG;
    write_params : TC_INT;
    p : PBIGNUM;
    q : PBIGNUM;
    g : PBIGNUM;
    pub_key : PBIGNUM;
    priv_key : PBIGNUM;
    kinv : BIGNUM;
    r : PBIGNUM;
    flags : TC_INT;
    method_mont_p : PBN_MONT_CTX;
    references : TC_INT;
    ex_data : CRYPTO_EX_DATA;
    meth : PDSA_METHOD;
    engine : PENGINE;
  end;
{$ENDREGION}

{$REGION 'EVP'}

  PEVP_PKEY_CTX = ^EVP_PKEY_CTX;
  PPEVP_PKEY_CTX = ^PEVP_PKEY_CTX;

  EVP_SIGN_METHOD = function(_type: TC_INT; m: PAnsiChar; m_length: TC_UINT; sigret: PAnsiChar; var siglen: TC_UINT; key: Pointer): TC_INT; cdecl;
  EVP_VERIFY_METHOD = function(_type: TC_INT; m: PAnsiChar; m_length: TC_UINT; sigbuf: PAnsiChar; siglen: TC_UINT; key: Pointer): TC_INT; cdecl;
  EVP_PKEY_gen_cb = function(ctx: PEVP_PKEY_CTX): TC_INT; cdecl;

  EVP_PKEY_CTX = record
      pmeth: PEVP_PKEY_METHOD;
      engine: PENGINE;
      pkey: PEVP_PKEY;
      peerkey: PEVP_PKEY;
      operation: TC_INT;
    data: Pointer;
      app_data: Pointer;
      pkey_gencb: EVP_PKEY_gen_cb;
      keygen_info: PC_INT;
    keygen_info_count: TC_INT;
    end;

    EVP_PKEY_METHOD = record
        _pkey_id: TC_INT;
        _flags: TC_INT;

        init: function(_ctx: PEVP_PKEY_CTX): TC_INT; cdecl;
        copy: function(_dst: PEVP_PKEY_CTX; _src: PEVP_PKEY_CTX): TC_INT; cdecl;
        cleanup: procedure(_ctx: PEVP_PKEY_CTX); cdecl;

        paramgen_init: function(_ctx: PEVP_PKEY_CTX): TC_INT; cdecl;
        paramgen: function(_ctx: PEVP_PKEY_CTX; _pkey: PEVP_PKEY): TC_INT; cdecl;

        keygen_init: function(_ctx: PEVP_PKEY_CTX): TC_INT; cdecl;
        keygen: function(_ctx: PEVP_PKEY_CTX; _pkey: PEVP_PKEY): TC_INT; cdecl;

        sign_init: function(_ctx: PEVP_PKEY_CTX): TC_INT; cdecl;
        sign: function(_ctx: PEVP_PKEY_CTX; _sig: PAnsiChar; var _siglen: TC_SIZE_T;const _tbs: PAnsiChar; _tbslen: TC_SIZE_T): TC_INT; cdecl;

        verify_init: function(_ctx: PEVP_PKEY_CTX): TC_INT; cdecl;
        verify: function(_ctx: PEVP_PKEY_CTX;const _sig: PAnsiChar; _siglen: TC_SIZE_T; const _tbs: PAnsiChar; _tbslen: TC_SIZE_T): TC_INT; cdecl;

        verify_recover_init: function(_ctx: PEVP_PKEY_CTX): TC_INT; cdecl;
        verify_recover: function(_ctx: PEVP_PKEY_CTX;_rout: PAnsiChar; var _routlen: TC_SIZE_T;const _sig: PAnsiChar; _siglen: TC_SIZE_T): TC_INT; cdecl;

        signctx_init: function(_ctx: PEVP_PKEY_CTX; _mctx: PEVP_MD_CTX): TC_INT; cdecl;
        signctx: function(_ctx: PEVP_PKEY_CTX; _sig: PAnsiChar; var siglen: TC_SIZE_T; _mctx: PEVP_MD_CTX): TC_INT; cdecl;

        verifyctx_init: function(_ctx: PEVP_PKEY_CTX; _mctx: PEVP_MD_CTX): TC_INT; cdecl;
        verifyctx: function(_ctx: PEVP_PKEY_CTX; const _sig: PAnsiChar;_siglen: TC_INT;_mctx: PEVP_MD_CTX): TC_INT; cdecl;

        encrypt_init: function(_ctx: PEVP_PKEY_CTX): TC_INT; cdecl;
        encrypt: function(_ctx: PEVP_PKEY_CTX; _out: PAnsiChar; var _outlen: TC_SIZE_T;const _in: PAnsiChar; _inlen: TC_SIZE_T): TC_INT; cdecl;

        decrypt_init: function(_ctx: PEVP_PKEY_CTX): TC_INT; cdecl;
        decrypt: function(_ctx: PEVP_PKEY_CTX; _out: PAnsiChar; var _outlen: TC_SIZE_T;const _in: PAnsiChar; _inlen: TC_SIZE_T): TC_INT; cdecl;

        derive_init: function(_ctx: PEVP_PKEY_CTX): TC_INT; cdecl;
        derive: function(_ctx: PEVP_PKEY_CTX; _key: PAnsiChar; var _keylen: TC_SIZE_T): TC_INT; cdecl;

        ctrl: function(_ctx: PEVP_PKEY_CTX; _type: TC_INT; _p1: TC_INT; _p2: Pointer): TC_INT; cdecl;
        ctrl_str: function(_ctx: PEVP_PKEY_CTX; const _type: PAnsiChar; const _value: PAnsiChar): TC_INT; cdecl;

    end; { record }



  EVP_PKEY_union = record
    case byte of
      0: (ptr : PAnsiChar);
      1: (rsa : PRSA);    // RSA
      2: (dsa : PDSA);    // DSA
      3: (dh :PDH);       // DH
      4: (ec : PEC_KEY);  // ECC
  end;

  STACK_OF_X509_ATTRIBUTE = record
    _stack: STACK;
  end;
  PSTACK_OF_X509_ATTRIBUTE = ^STACK_OF_X509_ATTRIBUTE;
  PPSTACK_OF_X509_ATTRIBUTE = ^PSTACK_OF_X509_ATTRIBUTE;

  PPEVP_PKEY = ^PEVP_PKEY;
  EVP_PKEY = record
    _type : TC_INT;
    save_type : TC_INT;
    references : TC_INT;
    ameth : PEVP_PKEY_ASN1_METHOD;
    enigne: PENGINE;
    pkey : EVP_PKEY_union;
    save_parameters: TC_INT;
    attributes : PSTACK_OF_X509_ATTRIBUTE;
  end;

    EVP_PKEY_ASN1_METHOD = record
        _pkey_id: TC_INT;
        _pkey_base_id: TC_INT;
        _pkey_flags: TC_ULONG;

        _pem_str: PAnsiChar;
        _info: PAnsiChar;

        pub_decode: function(_pk: PEVP_PKEY; _pub: PX509_PUBKEY): TC_INT; cdecl;
        pub_encode: function(_pub: PX509_PUBKEY; const _pk: PEVP_PKEY): TC_INT; cdecl;
        pub_cmp: function(const _a: PEVP_PKEY; const _b: PEVP_PKEY): TC_INT; cdecl;
        pub_print: function(_out: PBIO; const _pkey: PEVP_PKEY; _indent: TC_INT;_pctx: PASN1_PCTX): TC_INT; cdecl;

        priv_decode: function(_pk: PEVP_PKEY; _p8inf: PPKCS8_PRIV_KEY_INFO): TC_INT; cdecl;
        priv_encode: function(_p8: PPKCS8_PRIV_KEY_INFO; const _pk: PEVP_PKEY): TC_INT; cdecl;
        priv_print: function(_out: PBIO; const _pkey: PEVP_PKEY; _indent: TC_INT;_pctx: PASN1_PCTX): TC_INT; cdecl;

        pkey_size: function(const _pk: PEVP_PKEY): TC_INT; cdecl;
        pkey_bits: function(const _pk: PEVP_PKEY): TC_INT; cdecl;

        param_decode: function(_pkey: PEVP_PKEY;const _pder: PPAnsiChar; _derlen: TC_INT): TC_INT; cdecl;
        param_encode: function(const _pkey: PEVP_PKEY; _pder: PPAnsiChar): TC_INT; cdecl;
        param_missing: function(const _pk: PEVP_PKEY): TC_INT; cdecl;
        param_copy: function(_to: PEVP_PKEY; const _from: PEVP_PKEY): TC_INT; cdecl;
        param_cmp: function(const _a: PEVP_PKEY; const _b: PEVP_PKEY): TC_INT; cdecl;
        param_print: function(_out: PBIO; const _pkey: PEVP_PKEY; _indent: TC_INT; _pctx: PASN1_PCTX): TC_INT; cdecl;
        sig_print: function(_out: PBIO;const _sigalg: PX509_ALGOR; const _sig: PASN1_STRING;_indent: TC_INT; _pctx: PASN1_PCTX): TC_INT; cdecl;


        pkey_free: procedure(_pkey: PEVP_PKEY); cdecl;
        pkey_ctrl: function(_pkey: PEVP_PKEY; _op: TC_INT; _arg1: TC_LONG; _arg2: Pointer): TC_INT; cdecl;

        old_priv_decode: function(_pkey: Pointer;const _pder: PPAnsiChar; _derlen: TC_INT): TC_INT; cdecl;
        old_priv_encode: function(const _pkey: PEVP_PKEY; _pder: PPAnsiChar): TC_INT; cdecl;
        item_verify: function(_ctx: PEVP_MD_CTX; const _it: PASN1_ITEM; _asn: Pvoid; _a: PX509_ALGOR; _sig: PASN1_BIT_STRING; _pkey: PEVP_PKEY): TC_INT; cdecl;
        item_sign: function(_ctx: PEVP_MD_CTX; const _it: PASN1_ITEM; _asn: Pointer; _alg1: PX509_ALGOR; _alg2: PX509_ALGOR; _sig: PASN1_BIT_STRING): TC_INT; cdecl;
    end; { record }

  PEVP_CIPHER_CTX = ^EVP_CIPHER_CTX;
  EVP_CIPHER = record
    nid : TC_Int;
    block_size : TC_Int;
    key_len : TC_Int;
    iv_len : TC_Int;
    flags : TC_ULONG;
    init : function (ctx : PEVP_CIPHER_CTX; key : PAnsiChar; iv : PAnsiChar; enc : TC_Int): TC_Int; cdecl;
    do_cipher : function (ctx : PEVP_CIPHER_CTX; _out : PAnsiChar; _in : PAnsiChar; inl : size_t) : TC_Int; cdecl;
    cleanup : function (_para1 : PEVP_CIPHER_CTX): TC_Int; cdecl;
    ctx_size : TC_Int;
    set_asn1_parameters : function (_para1 : PEVP_CIPHER_CTX; _para2 : PASN1_TYPE) : TC_Int; cdecl;
    get_asn1_parameters :function (_para1 : PEVP_CIPHER_CTX; _para2 :  PASN1_TYPE) : TC_Int; cdecl;
    ctrl : function (_para1 : PEVP_CIPHER_CTX; _type : TC_Int; arg : TC_Int;  ptr : Pointer): TC_Int; cdecl;
    app_data : Pointer;
  end;

  EVP_CIPHER_DO = procedure(ciph: PEVP_CIPHER; from: PAnsiChar; _to: PAnsiChar; x: Pointer); cdecl;
  EVP_MD_DO = procedure(ciph: PEVP_MD; from: PAnsiChar; _to: PAnsiChar; x: Pointer); cdecl;

  EVP_CIPHER_CTX = record
    cipher : PEVP_CIPHER;
    engine : PENGINE;
    encrypt: TC_INT;
    buf_len : TC_INT;
    oiv : array [0..EVP_MAX_IV_LENGTH-1] of AnsiChar;
    iv : array [0..EVP_MAX_IV_LENGTH -1] of AnsiChar;
    buf : array [0..EVP_MAX_BLOCK_LENGTH -1] of AnsiChar;
    num : TC_INT;
    app_data : Pointer;
    key_len : TC_INT;
    flags : TC_ULONG;
    cipher_data : Pointer;
    final_used : TC_INT;
    block_mask : TC_INT;
    _final : array [0..EVP_MAX_BLOCK_LENGTH-1] of AnsiChar;
  end;


  EVP_CIPHER_INFO = record
    cipher : PEVP_CIPHER;
    iv : array [0..EVP_MAX_IV_LENGTH -1] of AnsiChar;
  end;


  EVP_MD_CTX = record
    digest : PEVP_MD;
    engine : PENGINE;
    flags : TC_ULONG;
    md_data : Pointer;
    pctx : PEVP_PKEY_CTX;
    update : function (ctx : PEVP_MD_CTX; const data : Pointer; count : size_t) : TC_INT cdecl;
  end;


  EVP_MD = record
    _type : TC_Int;
    pkey_type : TC_Int;
    md_size : TC_Int;
    flags : TC_ULONG;
    init : function (ctx : PEVP_MD_CTX) : TC_Int; cdecl;
    update : function (ctx : PEVP_MD_CTX; data : Pointer; count : size_t):TC_Int; cdecl;
    _final : function (ctx : PEVP_MD_CTX; md : PAnsiChar) : TC_Int; cdecl;
    copy : function (_to : PEVP_MD_CTX; from : PEVP_MD_CTX ) : TC_Int; cdecl;
    cleanup : function(ctx : PEVP_MD_CTX) : TC_Int; cdecl;
    sign : function(_type : TC_Int; m : PAnsiChar; m_length : TC_UINT;  sigret : PAnsiChar; siglen : TC_UINT; key : Pointer) : TC_Int; cdecl;
    verify : function(_type : TC_Int; m : PAnsiChar; m_length : PAnsiChar;  sigbuf : PAnsiChar; siglen : TC_UINT; key : Pointer) : TC_Int; cdecl;
    required_pkey_type : array [0..4] of TC_Int; // EVP_PKEY_xxx
    block_size : TC_Int;
    ctx_size : TC_Int;
    md_ctrl: function( ctx: PEVP_MD_CTX; cmd: TC_INT; p1: TC_INT; p2: Pointer): TC_INT; cdecl;
  end;

  PEVP_ENCODE_CTX = ^EVP_ENCODE_CTX;
  EVP_ENCODE_CTX = record
    num: TC_INT;
    _length: TC_INT;
    enc_data: array[0..79] of AnsiChar;
    line_num: TC_INT;
    expect_nl: TC_INT;
  end;

  EVP_PBE_KEYGEN = function(ctx: PEVP_CIPHER_CTX; pass: PAnsiChar; passlen: TC_INT; param: PASN1_TYPE; cipher: PEVP_CIPHER; md: PEVP_MD; en_de: TC_INT): TC_INT;
  PEVP_PBE_KEYGEN = ^EVP_PBE_KEYGEN;


{$ENDREGION}



{$REGION 'X509'}

  PPX509 = ^PX509;
  PX509_REQ = ^X509_REQ;
  PX509_CRL = ^X509_CRL;
  PX509_NAME = ^X509_NAME;
  PPX509_NAME = ^PX509_NAME;
  PX509_NAME_ENTRY = ^X509_NAME_ENTRY;
  PPX509_NAME_ENTRY = ^PX509_NAME_ENTRY;
  PX509_REQ_INFO = ^X509_REQ_INFO;
  PPX509_REQ_INFO = ^PX509_REQ_INFO;
  PX509_POLICY_CACHE = ^X509_POLICY_CACHE;
  PX509_CRL_METHOD = Pointer;
  PSTACK_OF_X509_REVOKED = PSTACK_OF;


  PPSTACK_OF_X509 = ^PSTACK_OF_X509;

  PSTACK_OF_X509_NAME_ENTRY = PSTACK_OF;

  PX509_OBJECTS = ^X509_OBJECTS;
  X509_OBJECTS = record
    nid: TC_INT;
    a2i: function: TC_INT; cdecl;
    i2a: function: TC_INT; cdecl;
  end;

  X509_HASH_DIR_CTX = record
    num_dirs : TC_INT;
    dirs : PPAnsiChar;
    dirs_type : PC_INT;
    num_dirs_alloced : TC_INT;
  end;
  PX509_HASH_DIR_CTX = ^X509_HASH_DIR_CTX;

  X509_CERT_FILE_CTX = record
    num_paths : TC_INT;  // number of paths to files or directories
    num_alloced : TC_INT;
    paths : PPAnsiChar;  // the list of paths or directories
    path_type : TC_INT;
  end;
  PX509_CERT_FILE_CTX = ^X509_CERT_FILE_CTX;

  x509_object_union = record
    case byte of
      0: (ptr : PAnsiChar);
      1: (_x509 : Px509);
      2: (crl : PX509_CRL);
      3: (pkey : PEVP_PKEY);
  end;

  X509_OBJECT = record
    _type : TC_Int;
    data : x509_object_union;
  end;
  PX509_OBJECT  = ^X509_OBJECT;
  PPX509_OBJECT  = ^PX509_OBJECT;

  PSTACK_OF_X509_OBJECT = PSTACK;

  X509_ALGOR = record
    algorithm : PASN1_OBJECT;
    parameter : PASN1_TYPE;
  end;

  PSTACK_OF_X509_ALGOR = PSTACK;
  PPSTACK_OF_X509_ALGOR = ^PSTACK_OF_X509_ALGOR;

  X509_VAL = record
    notBefore : PASN1_TIME;
    notAfter : PASN1_TIME;
  end;
  PX509_VAL = ^X509_VAL;
  PPX509_VAL =^PX509_VAL;

  X509_PUBKEY = record
    algor : PX509_ALGOR;
    public_key : PASN1_BIT_STRING;
    pkey : PEVP_PKEY;
  end;

  PPX509_PUBKEY =^PX509_PUBKEY;

  X509_SIG = record
    algor : PX509_ALGOR;
    digest : PASN1_OCTET_STRING;
  end;
  PX509_SIG = X509_SIG;
  PPX509_SIG =^PX509_SIG;

  X509_NAME_ENTRY = record
    _object : PASN1_OBJECT;
    value : PASN1_STRING;
    _set : TC_Int;
    size : TC_Int;
  end;

  X509_NAME = record
    entries : PSTACK_OF_X509_NAME_ENTRY;
    modified : TC_Int;
    bytes : PBUF_MEM;
    canon_enc: PAnsiChar;
    canon_enclen: TC_INT;
  end;

  X509_EXTENSION = record
    _object : PASN1_OBJECT;
    critical : ASN1_BOOLEAN;
    value : PASN1_OCTET_STRING;
  end;
  PX509_EXTENSION = ^X509_EXTENSION;
  PPX509_EXTENSION =^PX509_EXTENSION;

  PSTACK_OF_X509_EXTENSION = PSTACK;
  PPSTACK_OF_X509_EXTENSION = ^PSTACK_OF_X509_EXTENSION;
  PX509_EXTENSIONS = PPSTACK_OF_X509_EXTENSION;

  x509_attributes_union = record
    case Byte of
      $FF :(Ptr : PAnsiChar);
      0  : (_set: PSTACK_OF_ASN1_TYPE); // 0
      1  : (_single: PASN1_TYPE);
  end;

  X509_ATTRIBUTE = record
    _object : PASN1_OBJECT;
    single : TC_Int;
    value : x509_attributes_union;
  end;
  PX509_ATTRIBUTE = ^X509_ATTRIBUTE;
  PPX509_ATTRIBUTE = ^PX509_ATTRIBUTE;

  X509_REQ_INFO = record
    enc: ASN1_ENCODING;
    version: PASN1_INTEGER;
    subject: PX509_NAME;
    pubkey: PX509_PUBKEY;
    attributes: PSTACK_OF_X509_ATTRIBUTE;
  end;

  X509_REQ = record
    req_info: PX509_REQ_INFO;
    sig_alg: PX509_ALGOR;
    signature: PASN1_BIT_STRING;
    references: TC_Int;
  end;
  PPX509_REQ = ^PX509_REQ;


  X509_CINF = record
    version: PASN1_INTEGER;
    serialNumber: PASN1_INTEGER;
    signature: PX509_ALGOR;
    issuer: PX509_NAME;
    validity: PX509_VAL;
    subject: PX509_NAME;
    key: PX509_PUBKEY;
    issuerUID: PASN1_BIT_STRING;
    subjectUID: PASN1_BIT_STRING;
    extensions: PSTACK_OF_X509_EXTENSION;
    enc : ASN1_ENCODING;
  end;
  PX509_CINF = ^X509_CINF;
  PPX509_CINF = ^PX509_CINF;


  X509_CERT_AUX = record
    trust : PSTACK_OF_ASN1_OBJECT;
    reject : PSTACK_OF_ASN1_OBJECT;
    alias : PASN1_UTF8STRING;
    keyid : PASN1_OCTET_STRING;
    other : PSTACK_OF_X509_ALGOR;
  end;
  PX509_CERT_AUX = ^X509_CERT_AUX;
  PPX509_CERT_AUX = ^PX509_CERT_AUX;

  X509 = record
    cert_info: PX509_CINF;
    sig_alg : PX509_ALGOR;
    signature : PASN1_BIT_STRING;
    valid : TC_Int;
    references : TC_Int;
    name : PAnsiChar;
    ex_data : CRYPTO_EX_DATA;
    ex_pathlen : TC_LONG;
    ex_pcpathlen : TC_LONG;
    ex_flags : TC_ULONG;
    ex_kusage : TC_ULONG;
    ex_xkusage : TC_ULONG;
    ex_nscert : TC_ULONG;
    skid : PASN1_OCTET_STRING;
    akid : PAUTHORITY_KEYID;
    policy_cache : PX509_POLICY_CACHE;
    crldp: PSTACK_OF;
    altName: PSTACK_OF;
    nc: Pointer;
    rfc3779_addr : PSTACK_OF_IPAddressFamily;
    rfc3779_asid : PASIdentifiers;
    sha1_hash : array [0..SHA_DIGEST_LENGTH-1] of AnsiChar;
    aux : PX509_CERT_AUX;
  end;

  X509_CRL_INFO = record
    version : PASN1_INTEGER;
    sig_alg : PX509_ALGOR;
    issuer : PX509_NAME;
    lastUpdate : PASN1_TIME;
    nextUpdate : PASN1_TIME;
    revoked : PSTACK_OF_X509_REVOKED;
    extensions : PSTACK_OF_X509_EXTENSION; // [0]
    enc : ASN1_ENCODING;
  end;

  PX509_CRL_INFO     = ^X509_CRL_INFO;
  PPX509_CRL_INFO    =^PX509_CRL_INFO;
  PSTACK_OF_X509_CRL_INFO = PSTACK;
  PX509_LOOKUP = ^X509_LOOKUP;
  PSTACK_OF_X509_LOOKUP = PSTACK;
  PX509_VERIFY_PARAM = ^X509_VERIFY_PARAM;
  PX509_STORE_CTX = ^X509_STORE_CTX;
  PPX509_CRL = ^PX509_CRL;
  X509_STORE = record
    cache : TC_Int;
    objs : PSTACK_OF_X509_OBJECT;
    get_cert_methods : PSTACK_OF_X509_LOOKUP;
    param : PX509_VERIFY_PARAM;
    verify : function (ctx : PX509_STORE_CTX) : TC_Int; cdecl;
    verify_cb : function (ok : TC_Int; ctx : PX509_STORE_CTX) : TC_Int; cdecl;
    get_issuer : function (issuer : PPX509; ctx : PX509_STORE_CTX; x : PX509) : TC_Int; cdecl;
    check_issued : function (ctx : PX509_STORE_CTX; x : PX509; issuer : PX509) : TC_Int; cdecl;
    check_revocation : function (ctx : PX509_STORE_CTX) : TC_Int; cdecl;
    get_crl : function (ctx : PX509_STORE_CTX; crl : PPX509_CRL; x : PX509) : TC_Int; cdecl;
    check_crl : function(ctx : PX509_STORE_CTX; crl : PX509_CRL) : TC_Int; cdecl;
    cert_crl : function(ctx : PX509_STORE_CTX; crl : PX509_CRL; x : PX509) : TC_Int; cdecl;
    cleanup : function(ctx : PX509_STORE_CTX) : TC_Int; cdecl;
    ex_data : CRYPTO_EX_DATA;
    references : TC_Int;
  end;
  PX509_STORE = ^X509_STORE;
  X509_CRL = record
    crl : PX509_CRL_INFO;
    sig_alg : PX509_ALGOR;
    signature : PASN1_BIT_STRING;
    references : TC_Int;
    flags: TC_INT;
    akid: PAUTHORITY_KEYID;
    idp: Pointer;
    idp_flags: TC_INT;
    idp_reason: TC_INT;
    crl_number: PASN1_INTEGER;
    base_crl_number: PASN1_INTEGER;
    sha1_hash: array[0..SHA_DIGEST_LENGTH-1] of AnsiChar;
    issuers: PSTACK_OF;
    meth: PX509_CRL_METHOD;
    meth_data: Pointer;
  end;
  PSTACK_OF_X509_CRL = PSTACK;

  X509_LOOKUP_METHOD = record
    name : PAnsiChar;
    new_item : function (ctx : PX509_LOOKUP): TC_Int; cdecl;
    free : procedure (ctx : PX509_LOOKUP); cdecl;
    init : function(ctx : PX509_LOOKUP) : TC_Int; cdecl;
    shutdown : function(ctx : PX509_LOOKUP) : TC_Int; cdecl;
    ctrl: function(ctx : PX509_LOOKUP; cmd : TC_Int; const argc : PAnsiChar; argl : TC_LONG; out ret : PAnsiChar ) : TC_Int; cdecl;
    get_by_subject: function(ctx : PX509_LOOKUP; _type : TC_Int; name : PX509_NAME; ret : PX509_OBJECT ) : TC_Int; cdecl;
    get_by_issuer_serial : function(ctx : PX509_LOOKUP; _type : TC_Int; name : PX509_NAME; serial : PASN1_INTEGER; ret : PX509_OBJECT) : TC_Int; cdecl;
    get_by_fingerprint : function (ctx : PX509_LOOKUP; _type : TC_Int; bytes : PAnsiChar; len : TC_Int; ret : PX509_OBJECT): TC_Int; cdecl;
    get_by_alias : function(ctx : PX509_LOOKUP; _type : TC_Int; str : PAnsiChar; ret : PX509_OBJECT) : TC_Int; cdecl;
  end;
  PX509_LOOKUP_METHOD      = ^X509_LOOKUP_METHOD;
  PPX509_LOOKUP_METHOD     = ^PX509_LOOKUP_METHOD;

  X509_VERIFY_PARAM = record
    name : PAnsiChar;
    check_time : TC_time_t;
    inh_flags : TC_ULONG;
    flags : TC_ULONG;
    purpose : TC_Int;
    trust : TC_Int;
    depth : TC_Int;
    policies : PSTACK_OF_ASN1_OBJECT;
  end;
  PSTACK_OF_X509_VERIFY_PARAM = PSTACK;

  X509_LOOKUP = record
    init : TC_Int;
    skip : TC_Int;
    method : PX509_LOOKUP_METHOD;
    method_data : PAnsiChar;
    store_ctx : PX509_STORE;
  end;
  PPSTACK_OF_X509_LOOKUP = ^PSTACK_OF_X509_LOOKUP;

  X509_STORE_CTX = record
    ctx : PX509_STORE;
    current_method : TC_Int;
    cert : PX509;
    untrusted : PSTACK_OF_X509;
    crls : PSTACK_OF_X509_CRL;
    param : PX509_VERIFY_PARAM;
    other_ctx : Pointer;
    verify : function (ctx : PX509_STORE_CTX) : TC_Int; cdecl;
    verify_cb : function (ok : TC_Int; ctx : PX509_STORE_CTX) : TC_Int; cdecl;
    get_issuer : function (var issuer : PX509; ctx : PX509_STORE_CTX; x : PX509) : TC_Int; cdecl;
    check_issued : function(ctx : PX509_STORE_CTX; x, issuer : PX509) : TC_Int; cdecl;
    check_revocation : function (ctx : PX509_STORE_CTX): TC_Int; cdecl;
    get_crl : function (ctx : PX509_STORE_CTX; var crl : X509_CRL; x : PX509): TC_Int; cdecl;
    check_crl : function (ctx : PX509_STORE_CTX; var crl : X509_CRL) : TC_Int; cdecl;
    cert_crl : function (ctx : PX509_STORE_CTX; crl : PX509_CRL; x : PX509) : TC_Int; cdecl;
    check_policy : function (ctx : PX509_STORE_CTX) : TC_Int;  cdecl;
    cleanup : function (ctx : PX509_STORE_CTX) : TC_Int;  cdecl;
  end;
  PX509_EXTENSION_METHOD   = Pointer;

  PX509_TRUST = ^X509_TRUST;
  X509_TRUST_check_trust = function(_para1 : PX509_TRUST; para2 : PX509; _para3 : TC_Int) : TC_Int; cdecl;
  X509_TRUST = record
    trust : TC_Int;
    flags : TC_Int;
    check_trust : X509_TRUST_check_trust;
    name : PAnsiChar;
    arg1 : TC_Int;
    arg2 : Pointer;
  end;
  PPX509_TRUST = ^PX509_TRUST;
  PSTACK_OF_509_TRUST = PSTACK;

  X509_REVOKED = record
    serialNumber: PASN1_INTEGER;
    revocationDate: PASN1_TIME;
    extensions: PSTACK_OF_X509_EXTENSION;
    issuer: PSTACK_OF;
    reason: TC_INT;
    sequence: TC_Int;
  end;
  PX509_REVOKED      = ^X509_REVOKED;
  PPX509_REVOKED     =^PX509_REVOKED;

  PX509_PKEY       = ^X509_PKEY;
  PPX509_PKEY      =^PX509_PKEY;
  X509_PKEY = record
    version: TC_INT;
    enc_algor: PX509_ALGOR;
    enc_pkey: PASN1_OCTET_STRING;
    dec_pkey: PEVP_PKEY;
    key_length: TC_INT;
    key_data: PAnsiChar;
    key_free: TC_INT;
    cipher: EVP_CIPHER_INFO;
    references: TC_INT;
  end;

  X509_INFO = record
    x509 : PX509;
    crl : PX509_CRL;
    x_pkey : PX509_PKEY;
    enc_cipher: EVP_CIPHER_INFO;
    enc_len: TC_Int;
    enc_data: PAnsiChar;
    references: TC_Int;
  end;
  PX509_INFO       = ^X509_INFO;
  PPX509_INFO      =^PX509_INFO;
  PSTACK_OF_X509_INFO = PSTACK;

  PX509_CERT_PAIR = ^X509_CERT_PAIR;
  X509_CERT_PAIR = record
    _forward: PX509;
    _reverse: PX509;
  end;
  PPX509_CERT_PAIR = ^PX509_CERT_PAIR;

  PKCS8_PRIV_KEY_INFO = record
        broken: TC_INT;
        version: PASN1_INTEGER;
        pkeyalg: PX509_ALGOR;
        pkey: PASN1_TYPE;
        attributes: PSTACK_OF_X509_ATTRIBUTE;
  end;


  NETSCAPE_SPKAC = record
    pubkey : PX509_PUBKEY;
    challenge : PASN1_IA5STRING;
  end;
  PNETSCAPE_SPKAC = ^NETSCAPE_SPKAC;
  PPNETSCAPE_SPKAC = ^PNETSCAPE_SPKAC;

  NETSCAPE_SPKI = record
    spkac : PNETSCAPE_SPKAC;
    sig_algor : PX509_ALGOR;
    signature : PASN1_BIT_STRING;
  end;
  PNETSCAPE_SPKI = ^NETSCAPE_SPKI;
  PPNETSCAPE_SPKI = ^PNETSCAPE_SPKI;

  NETSCAPE_CERT_SEQUENCE = record
    _type : PASN1_OBJECT;
    certs : PSTACK_OF_X509;
  end;
  PNETSCAPE_CERT_SEQUENCE = ^NETSCAPE_CERT_SEQUENCE;
  PPNETSCAPE_CERT_SEQUENCE = ^PNETSCAPE_CERT_SEQUENCE;

  PPBEPARAM = ^PBEPARAM;
  PBEPARAM = record
    salt: PASN1_OCTET_STRING;
    iter: PASN1_INTEGER;
  end;

  PPBE2PARAM = ^PBE2PARAM;
  PBE2PARAM = record
    keyfunc: PX509_ALGOR;
    encryption: PX509_ALGOR;
  end;

  PPBKDF2PARAM = ^PBKDF2PARAM;
  PBKDF2PARAM = record
    salt: PASN1_TYPE;
    iter: PASN1_INTEGER;
    keylength: PASN1_INTEGER;
    prf: PX509_ALGOR;
  end;

  EVP_pub_decode_t = function(pk: PEVP_PKEY; pub: PX509_PUBKEY): TC_INT; cdecl;
  EVP_pub_encode_t = function(pub: PX509_PUBKEY;  pk: PEVP_PKEY): TC_INT; cdecl;
  EVP_pub_cmp_t = function(const a: PEVP_PKEY; const b: PEVP_PKEY): TC_INT; cdecl;
  EVP_pub_print_t = function(_out: PBIO; const pkey: PEVP_PKEY; indent: TC_INT; pctx: ASN1_PCTX): TC_INT; cdecl;
  EVP_pkey_size_t = function(const pk: PEVP_PKEY): TC_INT; cdecl;
  EVP_pkey_bits_t = function(const pk: PEVP_PKEY): TC_INT; cdecl;

  EVP_priv_decode_t = function(pk: PEVP_PKEY; p8inf: PPKCS8_PRIV_KEY_INFO): TC_INT; cdecl;
  EVP_priv_encode_t = function(p8: PPKCS8_PRIV_KEY_INFO; const pk : PEVP_PKEY): TC_INT; cdecl;
  EVP_priv_print_t =  function(_out: PBIO; const pkey: PEVP_PKEY; indent: TC_INT; pctx: ASN1_PCTX): TC_INT; cdecl;

  EVP_param_decode_t = function(pkey: PEVP_PKEY;var pder: PAnsiChar; derlen: TC_INT): TC_INT; cdecl;
  EVP_param_encode_t = function(const pkey: PEVP_PKEY; var pder: PAnsiChar): TC_INT; cdecl;
  EVP_param_missing_t = function(const pk: PEVP_PKEY): TC_INT; cdecl;
  EVP_param_copy_t = function(_to: PEVP_PKEY; const _from: PEVP_PKEY): TC_INT; cdecl;
  EVP_param_cmp_t = function(const a: PEVP_PKEY; const b: PEVP_PKEY): TC_INT; cdecl;
  EVP_param_print_t = function(_out: PBIO; const pkey: PEVP_PKEY; indent: TC_INT;   pctx: ASN1_PCTX): TC_INT; cdecl;
  EVP_pkey_free_t = procedure(pkey: PEVP_PKEY); cdecl;
  EVP_pkey_ctrl_t = function(pkey: PEVP_PKEY; op:TC_INT;arg1: TC_LONG; arg2: Pointer): TC_INT; cdecl;
{$ENDREGION}

{$REGION 'X509V3}


  PX509V3_CONF_METHOD = ^X509V3_CONF_METHOD;
  X509V3_CONF_METHOD = record
    get_string : function(db : Pointer; section, value : PAnsiChar) : PAnsiChar; cdecl;
    get_section : function(db : Pointer; section : PAnsiChar) : PSTACK_OF_CONF_VALUE; cdecl;
    free_string : procedure(db : Pointer; _string : PAnsiChar); cdecl;
    free_section : procedure (db : Pointer; section : PSTACK_OF_CONF_VALUE);
  end;

  V3_EXT_CTX = record
    flags : TC_INT;
    issuer_cert : PX509;
    subject_cert : PX509;
    subject_req : PX509_REQ;
    crl : PX509_CRL;
    db_meth : X509V3_CONF_METHOD;
    db : Pointer;
  end;

  X509V3_CTX = V3_EXT_CTX;
  PX509V3_CTX = ^X509V3_CTX;

  POTHERNAME = ^OTHERNAME;
  PPOTHERNAME = ^POTHERNAME;
  OTHERNAME = record
    type_id: PASN1_OBJECT;
    value: PASN1_TYPE;
  end;

  PEDIPARTYNAME = ^EDIPARTYNAME;
  PPEDIPARTYNAME = ^PEDIPARTYNAME;
  EDIPARTYNAME = record
      nameAssigner: PASN1_STRING;
      partyName: PASN1_STRING;
  end;

  GENERAL_NAME_union = record
  case byte of
    0: (ptr: PAnsiChar);
    1: (otherName: POTHERNAME);
      2: (rfc822Name: PASN1_IA5STRING);
      3: (dNSName: PASN1_IA5STRING);
      4: (x400Address: PASN1_TYPE);
      5: (directoryName: PX509_NAME);
      6: (ediPartyName: PEDIPARTYNAME);
      7: (uniformResourceIdentifier: PASN1_IA5STRING);
      8: (iPAddress: PASN1_OCTET_STRING);
      9: (registeredID: PASN1_OBJECT);
      10: (ip: PASN1_OCTET_STRING);
      11: (dirn: PX509_NAME);
      12: (ia5: PASN1_IA5STRING);
    13: (rid: PASN1_OBJECT);
      14: (other: PASN1_TYPE);
  end;

  PGENERAL_NAME = ^GENERAL_NAME;
  PPGENERAL_NAME = ^PGENERAL_NAME;
  GENERAL_NAME = record
    _type: TC_INT;
    d: GENERAL_NAME_union;
  end;
  PX509V3_EXT_METHOD = ^X509V3_EXT_METHOD;

  X509V3_EXT_NEW_func = function: Pointer; cdecl;
  X509V3_EXT_FREE_func = procedure(p: Pointer); cdecl;
  X509V3_EXT_D2I_func = function(_par1: Pointer; var _par2: PAnsiChar; _par3: TC_LONG): Pointer; cdecl;
  X509V3_EXT_I2D_func = function(_par1: Pointer; var _par2: PAnsiChar): TC_INT; cdecl;
  X509V3_EXT_I2V_func = function(method: PX509V3_EXT_METHOD; ext: Pointer; extlist: PSTACK_OF_CONF_VALUE): PSTACK_OF_CONF_VALUE; cdecl;
  X509V3_EXT_V2I_func = function(method: PX509V3_EXT_METHOD; ctx: PX509V3_CTX; values: PSTACK_OF_CONF_VALUE): Pointer; cdecl;
  X509V3_EXT_I2S_func = function(method: PX509V3_EXT_METHOD; ext: Pointer): PAnsiChar; cdecl;
  X509V3_EXT_S2I_func = function(method: PX509V3_EXT_METHOD; ctx: PX509V3_CTX; str: PAnsiChar): Pointer; cdecl;
  X509V3_EXT_I2R_func = function(method: PX509V3_EXT_METHOD; ext: Pointer; _out: PBIO; indent: TC_INT): TC_INT; cdecl;
  X509V3_EXT_R2I_func = function(method: PX509V3_EXT_METHOD; ctx: PX509V3_CTX; str: PAnsiChar): Pointer; cdecl;

  V3_EXT_METHOD = record
    ext_nid: TC_INT;
    ext_flags: TC_INT;
    it: PASN1_ITEM_EXP;
    ext_new: X509V3_EXT_NEW_func;
    ext_free: X509V3_EXT_FREE_func;
    d2i: X509V3_EXT_D2I_func;
    i2d: X509V3_EXT_I2D_func;
    i2s: X509V3_EXT_I2S_func;
    s2i: X509V3_EXT_S2I_func;
    i2v: X509V3_EXT_I2V_func;
    v2i: X509V3_EXT_V2I_func;
    i2r: X509V3_EXT_I2R_func;
    r2i: X509V3_EXT_R2I_func;
    usr_data: Pointer;
  end;
  X509V3_EXT_METHOD = V3_EXT_METHOD;
  PSTACK_OF_X509V3_EXT_METHOD = PSTACK_OF;
  ENUMERATED_NAMES = BIT_STRING_BITNAME;
  PEXTENDED_KEY_USAGE = PSTACK_OF_ASN1_OBJECT;
  PPEXTENDED_KEY_USAGE = ^PEXTENDED_KEY_USAGE;

  PBASIC_CONSTRAINTS = ^BASIC_CONSTRAINTS;
  PPBASIC_CONSTRAINTS = ^PBASIC_CONSTRAINTS;
  BASIC_CONSTRAINTS = record
    ca: TC_INT;
    pathlen: PASN1_INTEGER;
  end;

  PPKEY_USAGE_PERIOD = ^PKEY_USAGE_PERIOD;
  PPPKEY_USAGE_PERIOD = ^PPKEY_USAGE_PERIOD;
  PKEY_USAGE_PERIOD = record
    notBefore: PASN1_GENERALIZEDTIME;
    notAfter: PASN1_GENERALIZEDTIME;
  end;

  PACCESS_DESCRIPTION = ^ACCESS_DESCRIPTION;
  PPACCESS_DESCRIPTION = ^PACCESS_DESCRIPTION;
  ACCESS_DESCRIPTION = record
      method: PASN1_OBJECT;
      location: PGENERAL_NAME;
  end;
  PSTACK_OF_ACCESS_DESCRIPTION = PSTACK_OF;
  PAUTHORITY_INFO_ACCESS = PSTACK_OF_ACCESS_DESCRIPTION;
  PPAUTHORITY_INFO_ACCESS = ^PAUTHORITY_INFO_ACCESS;

  DIST_POINT_NAME_union = record
  case Byte of
      0: (fullname: PGENERAL_NAMES);
      1: (relativename: PSTACK_OF_X509_NAME_ENTRY);
  end;

  PDIST_POINT_NAME = ^DIST_POINT_NAME;
  PPDIST_POINT_NAME = ^PDIST_POINT_NAME;
  DIST_POINT_NAME = record
    _type: TC_INT;
    name: DIST_POINT_NAME_union;
    dpname: PX509_NAME;
  end;

  PDIST_POINT = ^DIST_POINT;
  PPDIST_POINT = ^PDIST_POINT;
  DIST_POINT = record
    distpoint: PDIST_POINT_NAME;
    reasons: PASN1_BIT_STRING;
    CRLissuer: PGENERAL_NAMES;
    dp_reasons: TC_INT;
  end;
  PSTACK_OF_DIST_POINT = PSTACK_OF;
  PCRL_DIST_POINTS = PSTACK_OF_DIST_POINT;
  PPCRL_DIST_POINTS = ^PCRL_DIST_POINTS;

  PSXNETID = ^SXNETID;
  PPSXNETID = ^PSXNETID;
  SXNETID = record
      zone: PASN1_INTEGER;
      user: PASN1_OCTET_STRING;
  end;
  PSTACK_OF_SXNETID = PSTACK_OF;

  PSXNET = ^SXNET;
  PPSXNET = ^PSXNET;
  SXNET = record
      version: PASN1_INTEGER;
      ids: PSTACK_OF_SXNETID;
  end;

  PNOTICEREF = ^NOTICEREF;
  PPNOTICEREF = ^PNOTICEREF;
  NOTICEREF = record
      organization: PASN1_STRING;
      noticenos: PSTACK_OF_ASN1_INTEGER;
  end;

  PUSERNOTICE = ^USERNOTICE;
  PPUSERNOTICE = ^PUSERNOTICE;
  USERNOTICE = record
      noticeref: PNOTICEREF;
      exptext: PASN1_STRING;
  end;

  POLICYQUALINFO_union = record
        cpsuri: PASN1_IA5STRING;
        usernotice: PUSERNOTICE;
        other: PASN1_TYPE;
  end;

  PPOLICYQUALINFO = ^POLICYQUALINFO;
  PPPOLICYQUALINFO = ^PPOLICYQUALINFO;
  POLICYQUALINFO = record
      pqualid: PASN1_OBJECT;
    d: POLICYQUALINFO_union
  end;
  PSTACK_OF_POLICYQUALINFO = PSTACK_OF;

  PPOLICYINFO = ^POLICYINFO;
  PPPOLICYINFO = ^PPOLICYINFO;
  POLICYINFO =  record
      policyid: PASN1_OBJECT;
      qualifiers: PSTACK_OF_POLICYQUALINFO;
  end;
  PSTACK_OF_POLICYINFO = PSTACK_OF;
  PCERTIFICATEPOLICIES = PSTACK_OF_POLICYINFO;
  PPCERTIFICATEPOLICIES = ^PCERTIFICATEPOLICIES;

  PPOLICY_MAPPING = ^POLICY_MAPPING;
  POLICY_MAPPING = record
      issuerDomainPolicy: PASN1_OBJECT;
      subjectDomainPolicy: PASN1_OBJECT;
  end;
  PSTACK_OF_POLICY_MAPPING = PSTACK_OF;

  PGENERAL_SUBTREE = ^GENERAL_SUBTREE;
  GENERAL_SUBTREE = record
      base: PGENERAL_NAME;
      minimum: PASN1_INTEGER;
      maximum: PASN1_INTEGER;
  end;
  PSTACK_OF_GENERAL_SUBTREE = PSTACK_OF;

  PNAME_CONSTRAINTS = ^NAME_CONSTRAINTS;
  NAME_CONSTRAINTS = record
      permittedSubtrees: PSTACK_OF_GENERAL_SUBTREE;
      excludedSubtrees: PSTACK_OF_GENERAL_SUBTREE;
  end;

  PPOLICY_CONSTRAINTS = ^POLICY_CONSTRAINTS;
  POLICY_CONSTRAINTS = record
      requireExplicitPolicy: PASN1_INTEGER;
      inhibitPolicyMapping: PASN1_INTEGER;
  end;

  PPROXY_POLICY = ^PROXY_POLICY;
  PPPROXY_POLICY = ^PPROXY_POLICY;
  PROXY_POLICY = record
    policyLanguage: PASN1_OBJECT;
      policy: PASN1_OCTET_STRING;
  end;

  PPROXY_CERT_INFO_EXTENSION = ^PROXY_CERT_INFO_EXTENSION;
  PPPROXY_CERT_INFO_EXTENSION = ^PPROXY_CERT_INFO_EXTENSION;
  PROXY_CERT_INFO_EXTENSION = record
      pcPathLengthConstraint: PASN1_INTEGER;
      proxyPolicy: PPROXY_POLICY;
  end;

  PISSUING_DIST_POINT = ^ISSUING_DIST_POINT;
  PPISSUING_DIST_POINT = ^PISSUING_DIST_POINT;
  ISSUING_DIST_POINT = record
      distpoint: PDIST_POINT_NAME;
      onlyuser: TC_INT;
      onlyCA: TC_INT;
      onlysomereasons: PASN1_BIT_STRING;
      indirectCRL: TC_INT;
      onlyattr: TC_INT;
  end;



  PX509_PURPOSE = ^X509_PURPOSE;

  X509_CHECK_PURPOSE_FUNC = function(p: PX509_PURPOSE; _x509: PX509; _i: TC_INT): TC_INT; cdecl;
  X509_PURPOSE = record
      purpose: TC_INT;
      trust: TC_INT;
      flags: TC_INT;
      check_purpose: X509_CHECK_PURPOSE_FUNC;
      name: PAnsiChar;
      sname: PAnsiChar;
      usr_data: Pointer;
  end;
  PSTACK_OF_X509_PURPOSE = PSTACK_OF;

  PSTACK_OF_X509_POLICY_NODE = PSTACK_OF;
  PSTACK_OF_OPENSSL_STRING = PSTACK_OF;

  PX509_POLICY_DATA = ^X509_POLICY_DATA;
  X509_POLICY_DATA = record
      flags: TC_UINT;
      valid_policy: PASN1_OBJECT;
      qualifier_set: PSTACK_OF_POLICYQUALINFO;
      expected_policy_set: PSTACK_OF_ASN1_OBJECT;
  end;
  PSTACK_OF_X509_POLICY_DATA = PSTACK_OF;

  X509_POLICY_CACHE = record
      anyPolicy: PX509_POLICY_DATA;
      data: PSTACK_OF_X509_POLICY_DATA;
      any_skip: TC_LONG;
      explicit_skip: TC_LONG;
      map_skip: TC_LONG;
  end;


  PX509_POLICY_NODE = ^X509_POLICY_NODE;
  X509_POLICY_NODE = record
    data: PX509_POLICY_DATA;
      parent: PX509_POLICY_NODE;
      nchild: TC_INT;
  end;

  PX509_POLICY_LEVEL = ^X509_POLICY_LEVEL;
  X509_POLICY_LEVEL = record
      cert: PX509;
      nodes: PSTACK_OF_X509_POLICY_NODE;
      anyPolicy: pX509_POLICY_NODE;
      flags: TC_UINT;
  end;

  PX509_POLICY_TREE = ^X509_POLICY_TREE;
  X509_POLICY_TREE = record
      levels: PX509_POLICY_LEVEL;
      nlevel: TC_INT;
      extra_data: PSTACK_OF_X509_POLICY_DATA;
      auth_policies: PSTACK_OF_X509_POLICY_NODE;
      user_policies: PSTACK_OF_X509_POLICY_NODE;
      flags: TC_UINT;
  end;



{$ENDREGION}

{$REGION 'PEM'}

    pem_password_cb = function(buf: PAnsiString; size: TC_INT; rwflag: TC_INT; userdata: pointer): integer; cdecl;

{$ENDREGION}

{$REGION 'PKCS7'}

  PPKCS7 = ^PKCS7;
  PPPKCS7 = ^PPKCS7;
  PSTACK_OF_PKCS7_SIGNER_INFO = PSTACK;
  PSTACK_OF_PKCS7_RECIP_INFO = PSTACK;

  PKCS7_SIGNED = record
    version : PASN1_INTEGER;
    md_algs : PSTACK_OF_X509_ALGOR;
    cert : PSTACK_OF_X509;
    crl : PSTACK_OF_X509_CRL;
    signer_info : PSTACK_OF_PKCS7_SIGNER_INFO;
    contents : PPKCS7;
  end;
  PPKCS7_SIGNED = ^PKCS7_SIGNED;

  PKCS7_ENC_CONTENT = record
    content_type : PASN1_OBJECT;
    algorithm : PX509_ALGOR;
    enc_data : PASN1_OCTET_STRING;
    cipher : PEVP_CIPHER;
  end;
  PPKCS7_ENC_CONTENT = ^PKCS7_ENC_CONTENT;

  PKCS7_ENVELOPE = record
    version : PASN1_INTEGER;
    recipientinfo : PSTACK_OF_PKCS7_RECIP_INFO;
    enc_data : PPKCS7_ENC_CONTENT;
  end;
  PPKCS7_ENVELOPE = ^PKCS7_ENVELOPE;
  PPPKCS7_ENVELOPE = ^PPKCS7_ENVELOPE;

  PKCS7_SIGN_ENVELOPE = record
    version : PASN1_INTEGER;
    md_algs : PSTACK_OF_X509_ALGOR;
    cert : PSTACK_OF_X509;
    crl : PSTACK_OF_X509_CRL;
    signer_info : PSTACK_OF_PKCS7_SIGNER_INFO;
    enc_data : PPKCS7_ENC_CONTENT;
    recipientinfo : PSTACK_OF_PKCS7_RECIP_INFO;
  end;
  PPKCS7_SIGN_ENVELOPE = ^PKCS7_SIGN_ENVELOPE;
  PPPKCS7_SIGN_ENVELOPE = ^PPKCS7_SIGN_ENVELOPE;

  PKCS7_DIGEST = record
    version : PASN1_INTEGER;
    md : PX509_ALGOR;
    contents : PPKCS7;
    digest : PASN1_OCTET_STRING;
  end;
  PPKCS7_DIGEST = ^PKCS7_DIGEST;

  PKCS7_ENCRYPT = record
    version : PASN1_INTEGER;
    enc_data : PPKCS7_ENC_CONTENT;
  end;
  PPKCS7_ENCRYPT = ^PKCS7_ENCRYPT;
    PPPKCS7_ENCRYPT = ^PPKCS7_ENCRYPT;

  PKCS7_union = record
    case Integer of
      0 : (ptr : PAnsiChar);
      1 : (data : PASN1_OCTET_STRING);
      2 : (sign : PPKCS7_SIGNED);
      3 : (enveloped : PPKCS7_ENVELOPE);
      4 : (signed_and_enveloped : PPKCS7_SIGN_ENVELOPE);
      5 : (digest : PPKCS7_DIGEST);
      6 : (encrypted : PPKCS7_ENCRYPT);
  end;

  PKCS7 = record
    asn1 : PAnsiChar;
    length : TC_LONG;
    state : TC_INT;
    detached : TC_INT;
    _type : PASN1_OBJECT;
    d : PKCS7_union;
  end;
{$ENDREGION}

{$REGION 'AES'}


  AES_KEY = record
{$IFDEF AES_LONG}
    rd_key: array[0..(4*(AES_MAXNR + 1))-1] of TC_ULONG;
{$ELSE}
    rd_key: array[0..(4*(AES_MAXNR + 1))-1] of TC_UINT;
{$ENDIF}
   rounds: TC_INT;
  end;
  PAES_KEY = ^AES_KEY;

  aes_buf = array[0..AES_BLOCK_SIZE-1] of Char;

{$ENDREGION}

{$REGION 'BLOWFISH'}


  BF_KEY = record
    P: array [0..BF_ROUNDS+1] of BF_LONG;
    S: array [0..(4*256)-1] of BF_LONG;
  end;
  PBF_KEY = ^BF_KEY;

{$ENDREGION}

{$REGION 'CMAX'}


  CMAC_CTX = record
    cctx: EVP_CIPHER_CTX;
    k1: array[0..EVP_MAX_BLOCK_LENGTH - 1] of TC_UCHAR;
    k2: array[0..EVP_MAX_BLOCK_LENGTH - 1] of TC_UCHAR;
    tbl: array[0..EVP_MAX_BLOCK_LENGTH - 1] of TC_UCHAR;
    last_block: array[0..EVP_MAX_BLOCK_LENGTH - 1] of TC_UCHAR;
    nlast_block: TC_INT;
  end;
  PCMAC_CTX = ^CMAC_CTX;

{$ENDREGION}

{$REGION 'CMS'}

  STACK_OF_CMS_SignerInfo = STACK_OF;
  PSTACK_OF_CMS_SignerInfo = ^STACK_OF_CMS_SignerInfo;

  STACK_OF_CMS_CertificateChoices = STACK_OF;
  PSTACK_OF_CMS_CertificateChoices = ^STACK_OF_CMS_CertificateChoices;

  STACK_OF_CMS_RevocationInfoChoice = STACK_OF;
  PSTACK_OF_CMS_RevocationInfoChoice = ^STACK_OF_CMS_RevocationInfoChoice;

  STACK_OF_CMS_RecipientInfo = STACK_OF;
  PSTACK_OF_CMS_RecipientInfo = ^STACK_OF_CMS_RecipientInfo;

  STACK_OF_CMS_RecipientEncryptedKey = STACK_OF;
  PSTACK_OF_CMS_RecipientEncryptedKey = ^STACK_OF_CMS_RecipientEncryptedKey;

  PCMS_EncapsulatedContentInfo = ^CMS_EncapsulatedContentInfo;
  CMS_EncapsulatedContentInfo = record
    eContentType: PASN1_OBJECT;
    eContent : PASN1_OCTET_STRING;
    partial: TC_INT;
  end;

  CMS_SignedData = record
   version: TC_LONG;
   digestAlgoritm: PSTACK_OF;
   encapContentInfo: PCMS_EncapsulatedContentInfo;
   certificates: PSTACK_OF_CMS_CertificateChoices;
   signerInfos: PSTACK_OF_CMS_SignerInfo;
  end;
  PCMS_SignedData = ^CMS_SignedData;

  PCMS_OriginatorInfo = ^CMS_OriginatorInfo;
  CMS_OriginatorInfo = record
     certificates: PSTACK_OF_CMS_CertificateChoices;
     crls: PSTACK_OF_CMS_RevocationInfoChoice
  end;

  PCMS_EncryptedContentInfo = ^CMS_EncryptedContentInfo;
  CMS_EncryptedContentInfo = record
      contentType: PASN1_OBJECT;
      contentEncryptionAlgorithm: PX509_ALGOR;
      encryptedContent: PASN1_OCTET_STRING;
      cipher: PEVP_CIPHER;
    key: PAnsiChar;
      keylen: TC_SIZE_T;
      debug: TC_INT;
    end;

  PCMS_EnvelopedData = ^CMS_EnvelopedData;
  CMS_EnvelopedData = record
      version: TC_LONG;
    originatorInfo: PCMS_OriginatorInfo;
    recipientInfos: PSTACK_OF_CMS_RecipientInfo;
      encryptedContentInfo: PCMS_EncryptedContentInfo;
    unprotectedAttrs: PSTACK_OF_X509_ATTRIBUTE
  end;

  PCMS_DigestedData = ^CMS_DigestedData;
  CMS_DigestedData = record
      version: TC_LONG;
      digestAlgorithm: PX509_ALGOR;
      encapContentInfo: PCMS_EncapsulatedContentInfo;
      digest: PASN1_OCTET_STRING;
  end;

  PCMS_EncryptedData = ^CMS_EncryptedData;
  CMS_EncryptedData = record
    version: TC_LONG;
    encryptedContentInfo: PCMS_EncryptedContentInfo;
    unprotectedAttrs: PSTACK_OF_X509_ATTRIBUTE
  end;

  PCMS_AuthenticatedData = ^CMS_AuthenticatedData;
  CMS_AuthenticatedData = record
    version: TC_LONG;
      originatorInfo: PCMS_OriginatorInfo;
    recipientInfos: PSTACK_OF_CMS_RecipientInfo;
    macAlgorithm: PX509_ALGOR;
      digestAlgorithm: PX509_ALGOR;
      encapContentInfo: PCMS_EncapsulatedContentInfo;
      authAttrs: PSTACK_OF_X509_ATTRIBUTE;
      mac: PASN1_OCTET_STRING;
      unauthAttrs: PSTACK_OF_X509_ATTRIBUTE;
  end;

  PCMS_CompressedData = ^CMS_CompressedData;
  CMS_CompressedData = record
    version: TC_LONG;
    compressionAlgorithm: PX509_ALGOR;
    recipientInfos: PSTACK_OF_CMS_RecipientInfo;
    encapContentInfo: PCMS_EncapsulatedContentInfo;
  end;

  CMS_ContentInfo_union = record
  case byte  of
    0: (data: PASN1_OCTET_STRING);
    1: (signedData: PCMS_SignedData);
    2: (envelopedData: PCMS_EnvelopedData);
    3: (digestedData: PCMS_DigestedData);
    4: (encryptedData: PCMS_EncryptedData);
    5: (authenticatedData: PCMS_AuthenticatedData);
    6: (compressedData: PCMS_CompressedData);
    7: (other: PASN1_TYPE);
    8: (otherData: Pointer);
  end;

  PCMS_ContentInfo = ^CMS_ContentInfo;
    PPCMS_ContentInfo = ^PCMS_ContentInfo;
  CMS_ContentInfo = record
     contentType: PASN1_OBJECT;
     d: CMS_ContentInfo_union;
  end;

  PCMS_IssuerAndSerialNumber = ^CMS_IssuerAndSerialNumber;
  CMS_IssuerAndSerialNumber = record
      issuer: PX509_NAME;
    serialNumber: PASN1_INTEGER;
  end;

   CMS_SignerIdentifier_union = record
    case byte of
      0: (issuerAndSerialNumber: PCMS_IssuerAndSerialNumber);
      1: (subjectKeyIdentifier: PASN1_OCTET_STRING);
   end;

   PCMS_SignerIdentifier = ^CMS_SignerIdentifier;
   CMS_SignerIdentifier = record
       _type: TC_INT;
     d: CMS_SignerIdentifier_union;
   end;
   PCMS_RecipientIdentifier = ^CMS_RecipientIdentifier;
   CMS_RecipientIdentifier = CMS_SignerIdentifier;

     PCMS_SignerInfo = ^CMS_SignerInfo;
     PPCMS_SignerInfo = ^PCMS_SignerInfo;
   CMS_SignerInfo = record
     version: TC_LONG;
     sid: PCMS_SignerIdentifier;
     digestAlgorithm: PX509_ALGOR;
     signedAttrs: PSTACK_OF_X509_ATTRIBUTE;
     signatureAlgorithm: PX509_ALGOR;
     signature: PASN1_OCTET_STRING;
     unsignedAttrs: PSTACK_OF_X509_ATTRIBUTE;
     signer: PX509;
     pkey: PEVP_PKEY;
   end;

  PCMS_KeyTransRecipientInfo = ^CMS_KeyTransRecipientInfo;
  CMS_KeyTransRecipientInfo = record
    version: TC_LONG;
    rid: PCMS_RecipientIdentifier;
    keyEncryptionAlgorithm: PX509_ALGOR;
    encryptedKey: PASN1_OCTET_STRING;
    recip: PX509;
    pkey: EVP_PKEY;
  end;

  PCMS_OriginatorPublicKey = ^CMS_OriginatorPublicKey;
  CMS_OriginatorPublicKey = record
    algorithm: PX509_ALGOR;
    publicKey: PASN1_BIT_STRING;
  end;

  PCMS_OtherKeyAttribute= ^CMS_OtherKeyAttribute;
  CMS_OtherKeyAttribute = record
    keyAttrId: ASN1_OBJECT;
    keyAttr: ASN1_TYPE;
  end;


  PCMS_RecipientKeyIdentifier = ^CMS_RecipientKeyIdentifier;
  CMS_RecipientKeyIdentifier = record
    subjectKeyIdentifier: PASN1_OCTET_STRING;
    date: PASN1_GENERALIZEDTIME;
    other: PCMS_OtherKeyAttribute;
  end;

  CMS_KeyAgreeRecipientIdentifier_union = record
   case byte of
    0: (issuerAndSerialNumber: PCMS_IssuerAndSerialNumber);
    1: (rKeyId: PCMS_RecipientKeyIdentifier);
  end;

  PCMS_KeyAgreeRecipientIdentifier = ^CMS_KeyAgreeRecipientIdentifier;
  CMS_KeyAgreeRecipientIdentifier = record
       _type: TC_INT;
     d: CMS_KeyAgreeRecipientIdentifier_union;
  end;


  PCMS_RecipientEncryptedKey = ^CMS_RecipientEncryptedKey;
  CMS_RecipientEncryptedKey = record
    rid: PCMS_KeyAgreeRecipientIdentifier;
    encryptedKey: PASN1_OCTET_STRING;
  end;


  CMS_OriginatorIdentifierOrKey_union = record
   case Byte of
    0: (issuerAndSerialNumber: PCMS_IssuerAndSerialNumber);
    1: (subjectKeyIdentifier: PASN1_OCTET_STRING);
    2: (originatorKey: PCMS_OriginatorPublicKey);
  end;

  PCMS_OriginatorIdentifierOrKey = ^CMS_OriginatorIdentifierOrKey;
  CMS_OriginatorIdentifierOrKey = record
       _type: TC_INT;
     d: CMS_OriginatorIdentifierOrKey_union;
  end;

  PCMS_KeyAgreeRecipientInfo = ^CMS_KeyAgreeRecipientInfo;
  CMS_KeyAgreeRecipientInfo = record
    version: TC_LONG;
    originator: PCMS_OriginatorIdentifierOrKey;
    ukm: PASN1_OCTET_STRING;
    keyEncryptionAlgorithm: PX509_ALGOR;
    recipientEncryptedKeys: PSTACK_OF_CMS_RecipientEncryptedKey;
  end;

  PCMS_KEKIdentifier = ^CMS_KEKIdentifier;
  CMS_KEKIdentifier = record
    keyIdentifier: PASN1_OCTET_STRING;
    date: PASN1_GENERALIZEDTIME;
    other: PCMS_OtherKeyAttribute;
  end;

  PCMS_KEKRecipientInfo = ^CMS_KEKRecipientInfo;
  CMS_KEKRecipientInfo = record
    version: TC_LONG;
    kekid: CMS_KEKIdentifier;
    keyEncryptionAlgorithm: PX509_ALGOR;
    encryptedKey: PASN1_OCTET_STRING;
    key: PAnsiChar;
    keylen: TC_SIZE_T;
  end;

  PCMS_PasswordRecipientInfo = ^CMS_PasswordRecipientInfo;
  CMS_PasswordRecipientInfo = record
    version: TC_LONG;
    keyDerivationAlgorithm: PX509_ALGOR;
    keyEncryptionAlgorithm: PX509_ALGOR;
    encryptedKey: PASN1_OCTET_STRING;
    pass: PAnsiChar;
    passlen: TC_SIZE_T;
  end;

  PCMS_OtherRecipientInfo = ^CMS_OtherRecipientInfo;
  CMS_OtherRecipientInfo = record
    oriType: ASN1_OBJECT;
    oriValue: ASN1_TYPE;
  end;

  CMS_RecipientInfo_union = record
  case byte of
    0: (ktri: PCMS_KeyTransRecipientInfo);
    1: (kari: PCMS_KeyAgreeRecipientInfo);
    2: (kekri: PCMS_KEKRecipientInfo);
    3: (pwri: PCMS_PasswordRecipientInfo);
    4: (ori: PCMS_OtherRecipientInfo);
  end;

  PCMS_RecipientInfo = ^CMS_RecipientInfo;
  CMS_RecipientInfo = record
      _type: TC_INT;
    d: CMS_RecipientInfo_union;
  end;

  PCMS_OtherRevocationInfoFormat = ^CMS_OtherRevocationInfoFormat;
  CMS_OtherRevocationInfoFormat = record
    otherRevInfoFormat: PASN1_OBJECT;
    otherRevInfo: PASN1_TYPE;
  end;

  CMS_RevocationInfoChoice_union = record
   case byte of
    0: (crl: PX509_CRL);
    1: (other: PCMS_OtherRevocationInfoFormat);
  end;

  PCMS_RevocationInfoChoice = ^CMS_RevocationInfoChoice;
  CMS_RevocationInfoChoice = record
      _type: TC_INT;
      d:CMS_RevocationInfoChoice_union;
  end;

  PCMS_OtherCertificateFormat = ^CMS_OtherCertificateFormat;
  CMS_OtherCertificateFormat = record
    otherCertFormat: PASN1_OBJECT;
    otherCert: PASN1_TYPE;
  end;

  CMS_CertificateChoices_union = record
  case byte of
    0: (certificate: PX509);
    1: (extendedCertificate: PASN1_STRING);
    2: (v1AttrCert: PASN1_STRING);
    3: (v2AttrCert: PASN1_STRING);
    4: (other: PCMS_OtherCertificateFormat);
  end;

  PCMS_CertificateChoices = ^CMS_CertificateChoices;
  CMS_CertificateChoices = record
    _type: TC_INT;
    d:CMS_CertificateChoices_union;
  end;

    CMS_RECEIPTSFROM_union = record
    case byte of
        0: (_allOrFirstTier: TC_LONG);
        1: (_receiptList: PSTACK_OF_GENERAL_NAMES);
    end; { record }

    PCMS_RECEIPTSFROM = ^CMS_RECEIPTSFROM;
    PPCMS_RECEIPTSFROM = ^PCMS_RECEIPTSFROM;
    CMS_RECEIPTSFROM = record
        _type: TC_INT;
        d: CMS_RECEIPTSFROM_union;
    end; { record }

    PCMS_RECEIPTREQUEST = ^CMS_RECEIPTREQUEST;
    PPCMS_RECEIPTREQUEST = ^PCMS_RECEIPTREQUEST;
    CMS_RECEIPTREQUEST = record
        _signedContentIdentifier: PASN1_OCTET_STRING;
        _receiptsFrom: PCMS_ReceiptsFrom;
         _receiptsTo: PSTACK_OF_GENERAL_NAMES;
    end; { record }

{$ENDREGION}

{$REGION 'DES'}


  DES_cblock = array[0..7] of TC_UCHAR;
  PDES_cblock = ^DES_cblock;
  const_DES_cblock = array[0..7] of TC_UCHAR;
  Pconst_DES_cblock = ^const_DES_cblock;
  DES_cblock_array = array of DES_cblock;

  DES_key_schedule_union = record
  case Byte of
     0: (cblock: DES_cblock);
     1: (deslong: array[0..1] of DES_LONG);
  end;

  PDES_key_schedule = ^DES_key_schedule;
  DES_key_schedule = record
    ks: array[0..15] of DES_key_schedule_union;
  end;

{$ENDREGION}


{$REGION 'ENGINE'}
{$ENDREGION}

{$REGION 'RAND'}

    PRAND_METHOD = ^RAND_METHOD;
    RAND_METHOD = record
        seed: procedure(buf: Pointer; num: TC_INT); cdecl;
        bytes: function(buf: PAnsiChar; num: TC_INT): TC_INT; cdecl;
        cleanup: procedure; cdecl;
        add: procedure(buf: Pointer;num: TC_INT;entropy: Double); cdecl;
        pseudorand: function(buf: PAnsiChar; num: TC_INT): TC_INT; cdecl;
        status: function: TC_INT; cdecl;
    end;


{$ENDREGION}


  PECDH_METHOD = ^ECDH_METHOD;
  PECDSA_METHOD = ^ECDSA_METHOD;
  PSTORE_METHOD = Pointer;

{$REGION 'CAMELLIA'}

    KEY_TABLE_TYPE = array [0..CAMELLIA_TABLE_WORD_LEN-1] of TC_UINT;
    CAMELLIA_BUF = array[0..CAMELLIA_BLOCK_SIZE-1] of AnsiChar;
    CAMELLIA_KEY_union = record
     case byte of
        0: (d: double);
        1: (rd_key: KEY_TABLE_TYPE);
    end;

    PCAMELLIA_KEY = ^CAMELLIA_KEY;
    CAMELLIA_KEY = record
        u: CAMELLIA_KEY_union;
        grand_rounds: TC_INT;
    end;

{$ENDREGION}

{$REGION 'COMP'}

    PCOMP_CTX = ^COMP_CTX;
    PCOMP_METHOD = ^COMP_METHOD;
    COMP_METHOD = record
        _type: TC_INT;
        name: PAnsiChar;
        init: function(ctx: PCOMP_CTX): TC_INT; cdecl;
        finish: procedure(ctx: PCOMP_CTX); cdecl;
        compress: function(ctx: PCOMP_CTX; _out: PAnsiChar; olen: TC_UINT; _in: PAnsiChar; ilen: TC_UINT): TC_INT; cdecl;
        expand: function(ctx: PCOMP_CTX; _out: PAnsiChar; olen: TC_UINT; _in: PAnsiChar; ilen: TC_UINT): TC_INT; cdecl;
        ctrl: function: TC_LONG; cdecl;
        callback_ctrl: function: TC_LONG; cdecl;
    end;

    COMP_CTX = record
        meth: PCOMP_METHOD;
        compress_in: TC_ULONG;
        compress_out: TC_ULONG;
        expand_in: TC_ULONG;
        expand_out: TC_ULONG;
        ex_data: CRYPTO_EX_DATA;
    end;


{$ENDREGION}


  SK_POP_FREE_PROC = procedure(_par1: Pointer); cdecl;

  tm = record
    tm_sec: Integer;            // Seconds. [0-60] (1 leap second)
    tm_min: Integer;            // Minutes. [0-59]
    tm_hour: Integer;           // Hours.[0-23]
    tm_mday: Integer;           // Day.[1-31]
    tm_mon: Integer;            // Month.[0-11]
    tm_year: Integer;           // Year since 1900
    tm_wday: Integer;           // Day of week [0-6] (Sunday = 0)
    tm_yday: Integer;           // Days of year [0-365]
    tm_isdst: Integer;          // Daylight Savings flag [-1/0/1]
    tm_gmtoff: LongInt;         // Seconds east of UTC
    tm_zone: PAnsiChar;         // Timezone abbreviation
  end;
  Ptm = ^tm;

{$REGION 'PKCS12'}

    PPKCS12_MAC_DATA = ^PKCS12_MAC_DATA;
    PPPKCS12_MAC_DATA = ^PPKCS12_MAC_DATA;
    PKCS12_MAC_DATA = record
        dinfo: PX509_SIG;
        salt: PASN1_OCTET_STRING;
        iter: PASN1_INTEGER;
    end;

    PPKCS12 = ^PKCS12;
    PPPKCS12 = ^PPKCS12;
    PKCS12 = record
        version: PASN1_INTEGER;
        mac: PPKCS12_MAC_DATA;
        authsafes: PPKCS7;
    end;

    PKCS12_BAGS_union = record
    case byte of
     0: (x509cert: PASN1_OCTET_STRING);
     1: (x509crl: PASN1_OCTET_STRING);
     2: (octet: PASN1_OCTET_STRING);
     3: (sdsicert: PASN1_IA5STRING);
     4: (other: PASN1_TYPE);
    end; { record }

    PPKCS12_BAGS = ^PKCS12_BAGS;
    PPPKCS12_BAGS = ^PPKCS12_BAGS;
    PKCS12_BAGS = record
        _type: PASN1_OBJECT;
        value: PKCS12_BAGS_union;
    end; { record }

    PSTACK_OF_PKCS12_SAFEBAG = PSTACK_OF;
    PPSTACK_OF_PKCS12_SAFEBAG = ^PSTACK_OF_PKCS12_SAFEBAG;
    PPKCS12_SAFEBAG = ^PKCS12_SAFEBAG;
    PPPKCS12_SAFEBAG = ^PPKCS12_SAFEBAG;
    PKCS12_SAFEBAG_union = record
        bag: PPKCS12_BAGS;
        keybag: PPKCS8_PRIV_KEY_INFO;
        shkeybag: PX509_SIG;
        safes: PSTACK_OF_PKCS12_SAFEBAG;
        other: PASN1_TYPE;
    end; { record }

    PKCS12_SAFEBAG = record
        _type: PASN1_OBJECT;
        value: PKCS12_SAFEBAG_union;
        attrib: PSTACK_OF_X509_ATTRIBUTE;
    end; { record }
    PSTACK_OF_PKCS7 = PSTACK_OF;
    PPSTACK_OF_PKCS7 = ^PSTACK_OF_PKCS7;

    PPKCS7_ISSUER_AND_SERIAL = ^PKCS7_ISSUER_AND_SERIAL;
    PPPKCS7_ISSUER_AND_SERIAL = ^PPKCS7_ISSUER_AND_SERIAL;
    PKCS7_ISSUER_AND_SERIAL = record
        issuer: PX509_NAME;
        serial: PASN1_INTEGER;
    end; { record }

    PPKCS7_SIGNER_INFO = ^PKCS7_SIGNER_INFO;
    PPPKCS7_SIGNER_INFO = ^PPKCS7_SIGNER_INFO;
    PKCS7_SIGNER_INFO = record
        version: PASN1_INTEGER;
        issuer_and_serial: PPKCS7_ISSUER_AND_SERIAL;
        digest_alg: PX509_ALGOR;
        auth_attr: PSTACK_OF_X509_ATTRIBUTE;
        digest_enc_alg: PX509_ALGOR;
        enc_digest: PASN1_OCTET_STRING;
        unauth_attr: PSTACK_OF_X509_ATTRIBUTE;
        pkey: PEVP_PKEY;
    end; { record }


    PPKCS7_RECIP_INFO = ^PKCS7_RECIP_INFO;
    PPPKCS7_RECIP_INFO = ^PPKCS7_RECIP_INFO;
    PKCS7_RECIP_INFO = record
        _version: PASN1_INTEGER;
        _issuer_and_serial: PPKCS7_ISSUER_AND_SERIAL;
        _key_enc_algor: PX509_ALGOR;
        _enc_key: PASN1_OCTET_STRING;
        _cert: PX509;
    end; { record }

    PPPKCS7_SIGNED = ^PPKCS7_SIGNED;
    PSTACK_OF_PKCS7_SIGNED = PSTACK_OF;

    PPPKCS7_ENC_CONTENT = ^PPKCS7_ENC_CONTENT;

    PPKCS7_ENVELOPED = ^PKCS7_ENVELOPED;
    PPPKCS7_ENVELOPED = ^PPKCS7_ENVELOPED;
    PKCS7_ENVELOPED = record
        _version: PASN1_INTEGER;
        recipientinfo: PSTACK_OF_PKCS7_RECIP_INFO;
        _enc_data: PPKCS7_ENC_CONTENT;
    end; { record }

    PKCS7_SIGNEDANDENVELOPED = record
        _version: PASN1_INTEGER;
        _md_algs: PSTACK_OF_X509_ALGOR;
        _cert: PSTACK_OF_X509;
        _crl: PSTACK_OF_X509_CRL;
        _signer_info: PSTACK_OF_PKCS7_SIGNER_INFO;
        _enc_data: PPKCS7_ENC_CONTENT   ;
        _recipientinfo: PSTACK_OF_PKCS7_RECIP_INFO;
    end; { record }

    PPPKCS7_DIGEST = ^PPKCS7_DIGEST;

    PPKCS7_ENCRYPTED = ^PKCS7_ENCRYPTED;
    PPPKCS7_ENCRYPTED = ^PPKCS7_ENCRYPTED;
    PKCS7_ENCRYPTED = record
        _version: PASN1_INTEGER;
        _enc_data: PPKCS7_ENC_CONTENT;
    end; { record }

  PPKCS7_ATTR_SIGN = Pointer;
    PPPKCS7_ATTR_SIGN = ^PPKCS7_ATTR_SIGN;
    PPKCS7_ATTR_VERIFY = Pointer;
    PPPKCS7_ATTR_VERIFY = ^PPKCS7_ATTR_VERIFY;
{$ENDREGION}

{$REGION 'ECDH'}

    ecdh_kdf = function(const _in: Pointer; _inlen: TC_SIZE_T; _out: Pointer; var _outlen: TC_SIZE_T): pointer; cdecl;

    PPECDH_METHOD = ^PECDH_METHOD;
    ECDH_METHOD = record
        _name: PAnsiChar;
      _compute_key: function(_key: Pointer; _outlen: TC_SIZE_T; const _pub_key: PEC_POINT; _ecdh: PEC_KEY; kdf: ecdh_kdf): TC_INT; cdecl;
      _init: function(_eckey: PEC_KEY): TC_INT; cdecl;
      _finish: function(_eckey: PEC_KEY): TC_INT; cdecl;
      _flags: TC_INT;
      _app_data: PAnsiChar;
    end; { record }

{$ENDREGION}

{$REGION 'ECDSA'}

    PECDSA_SIG = ^ECDSA_SIG;
    PPECDSA_SIG = ^PECDSA_SIG;
    ECDSA_SIG = record
        _r: PBIGNUM;
        _s: PBIGNUM;
    end; { record }

    PPECDSA_METHOD = ^PECDSA_METHOD;
    ECDSA_METHOD = record
        _name: PAnsiChar;
        ecdsa_do_sign: function(const _dgst: PAnsiChar; _dgst_len: TC_INT; const _inv: PBIGNUM; const _rp: PBIGNUM; _eckey: PEC_KEY): PECDSA_SIG; cdecl;
        ecdsa_sign_setup: function(_eckey: PEC_KEY; _ctx: PBN_CTX; _kinv: PPBIGNUM ; _r: PPBIGNUM ): TC_INT; cdecl;
        ecdsa_do_verify: function(const _dgst: PAnsiChar; _dgst_len: TC_INT; const _sig: PPECDSA_SIG; _eckey: PPEC_KEY): TC_INT; cdecl;
        init: function(_eckey: PEC_KEY): TC_INT; cdecl;
        finish: function(_eckey: PEC_KEY): TC_INT; cdecl;
        _flags: TC_INT;
        _app_data: PAnsiChar;
    end; { record }

{$ENDREGION}

{$REGION 'HMAC'}
    PHMAC_CTX = ^HMAC_CTX;
    PPHMAC_CTX = ^PHMAC_CTX;
    HMAC_CTX = record
        _md: PEVP_MD;
        _md_ctx: EVP_MD_CTX;
        _i_ctx: EVP_MD_CTX;
        _o_ctx: EVP_MD_CTX;
        _key_length: TC_UINT;
        key: array[0..HMAC_MAX_MD_CBLOCK - 1] of ansichar;
    end; { record }

{$ENDREGION}


{$REGION 'IDEA'}

  PIDEA_KEY_SCHEDULE = ^IDEA_KEY_SCHEDULE;
    PPIDEA_KEY_SCHEDULE = ^PIDEA_KEY_SCHEDULE;
    IDEA_KEY_SCHEDULE = record
     _data: array[0..8,0..5] of IDEA_INT;
    end;

{$ENDREGION}

{$REGION 'MD4'}

    PMD4_CTX = ^MD4_CTX;
    PPMD4_CTX = ^PMD4_CTX;
    MD4_CTX = record
        A,B,C,D: MD4_LONG;
        Nl,Nh: MD4_LONG;
        data: array[0..MD4_LBLOCK - 1] of MD4_LONG;
        _num: TC_UINT;
    end; { record }

{$ENDREGION}

{$REGION 'MD5'}

    PMD5_CTX = ^MD5_CTX;
    PPMD5_CTX = ^PMD5_CTX;
    MD5_CTX = record
        A,B,C,D: MD5_LONG;
        Nl,Nh: MD5_LONG;
        data: array[0..MD5_LBLOCK-1] of MD5_LONG;
        _num: TC_UINT;
    end; { record }

{$ENDREGION}

{$REGION 'MDC2'}

    PMDC2_CTX = ^MDC2_CTX;
    PPMDC2_CTX = ^PMDC2_CTX;
    MDC2_CTX = record
        _num: TC_UINT;
        _data: array[0..MDC2_BLOCK-1] of AnsiChar;
        _h, _hh: DES_cblock;
        _pad_type: TC_INT;
    end; { record }

{$ENDREGION}

{$REGION 'OCSP}

    POCSP_CERTID = ^OCSP_CERTID;
    PPOCSP_CERTID = ^POCSP_CERTID;
    OCSP_CERTID = record
        _hashAlgorithm: PX509_ALGOR;
        _issuerNameHash: PASN1_OCTET_STRING;
        _issuerKeyHash: PASN1_OCTET_STRING;
        _serialNumber: PASN1_INTEGER;
    end; { record }
    PSTACK_OF_OCSP_CERT_ID = PSTACK_OF;

    POCSP_ONEREQ = ^OCSP_ONEREQ;
    PPOCSP_ONEREQ = ^POCSP_ONEREQ;
    OCSP_ONEREQ = record
        _reqCert: POCSP_CERTID;
        _singleRequestExtensions: PSTACK_OF_X509_EXTENSION;
    end; { record }
    PSTACK_OF_OCSP_ONEREQ = PSTACK_OF;

    POCSP_REQINFO = ^OCSP_REQINFO;
    PPOCSP_REQINFO = ^POCSP_REQINFO;
    OCSP_REQINFO = record
        _version: PASN1_INTEGER;
        _requestorName: PGENERAL_NAME;
        _requestList: PSTACK_OF_OCSP_ONEREQ;
        _requestExtensions: PSTACK_OF_X509_EXTENSION;
    end; { record }

    POCSP_SIGNATURE = ^OCSP_SIGNATURE;
    PPOCSP_SIGNATURE = ^POCSP_SIGNATURE;
    OCSP_SIGNATURE = record
        _signatureAlgorithm: PX509_ALGOR;
        _signature: PASN1_BIT_STRING;
        _certs: PSTACK_OF_X509;
    end; { record }

    POCSP_REQUEST = ^OCSP_REQUEST;
    PPOCSP_REQUEST = ^POCSP_REQUEST;
    OCSP_REQUEST = record
        _tbsRequest: POCSP_REQINFO;
        _optionalSignature: POCSP_SIGNATURE;
    end; { record }

    POCSP_RESPBYTES = ^OCSP_RESPBYTES;
    PPOCSP_RESPBYTES = ^POCSP_RESPBYTES;
    OCSP_RESPBYTES = record
        _responseType: PASN1_OBJECT;
        _response: PASN1_OCTET_STRING;
    end; { record }

    POCSP_RESPONSE = ^OCSP_RESPONSE;
    PPOCSP_RESPONSE = ^POCSP_RESPONSE;
    OCSP_RESPONSE = record
        _responseStatus: PASN1_ENUMERATED;
        _responseBytes: POCSP_RESPBYTES ;
    end; { record }

    OCSP_RESPONDER_ID_union = record
    case byte of
        0: (_byName: PX509_NAME);
        1: (_byKey: PASN1_OCTET_STRING);
    end; { record }

    POCSP_RESPID = ^OCSP_RESPID;
    PPOCSP_RESPID = ^POCSP_RESPID;
    OCSP_RESPID = record
        _type: TC_INT;
        _value: OCSP_RESPONDER_ID_union;
    end; { record }

    PSTACK_OF_OCSP_RESPID = PSTACK_OF;

    POCSP_REVOKEDINFO = ^OCSP_REVOKEDINFO;
    PPOCSP_REVOKEDINFO = ^POCSP_REVOKEDINFO;
    OCSP_REVOKEDINFO = record
        _revocationTime: PASN1_GENERALIZEDTIME;
        _revocationReason: PASN1_ENUMERATED;
    end; { record }

    OCSP_CERTSTATUS_union = record
    case byte of
        0: (_good: PASN1_NULL);
        1: (_revoked: POCSP_REVOKEDINFO);
        2: (_unknown: PASN1_NULL);
    end; { record }

    POCSP_CERTSTATUS = ^OCSP_CERTSTATUS;
    PPOCSP_CERTSTATUS = ^POCSP_CERTSTATUS;
    OCSP_CERTSTATUS = record
        _type: TC_INT;
        _value :OCSP_CERTSTATUS_union;
    end;

    POCSP_SINGLERESP = ^OCSP_SINGLERESP;
    PPOCSP_SINGLERESP = ^POCSP_SINGLERESP;
    OCSP_SINGLERESP = record
        _certId: POCSP_CERTID;
        _certStatus: POCSP_CERTSTATUS;
        _thisUpdate: PASN1_GENERALIZEDTIME;
        _nextUpdate: PASN1_GENERALIZEDTIME;
        _singleExtensions: PSTACK_OF_X509_EXTENSION;
    end; { record }
    PSTACK_OF_OCSP_SINGLERESP = PSTACK_OF;

    POCSP_RESPDATA = ^OCSP_RESPDATA;
    PPOCSP_RESPDATA = ^POCSP_RESPDATA;
    OCSP_RESPDATA = record
        _version: PASN1_INTEGER;
        _responderId: POCSP_RESPID ;
        _producedAt: PASN1_GENERALIZEDTIME;
        _responses: PSTACK_OF_OCSP_SINGLERESP;
        _responseExtensions: PSTACK_OF_X509_EXTENSION;
    end; { record }

    POCSP_BASICRESP = ^OCSP_BASICRESP;
    PPOCSP_BASICRESP = ^POCSP_BASICRESP;
    OCSP_BASICRESP = record
        _tbsResponseData: POCSP_RESPDATA;
        _signatureAlgorithm: PX509_ALGOR;
        _signature: PASN1_BIT_STRING;
        _certs: PSTACK_OF_X509;
    end; { record }

    POCSP_CRLID = ^OCSP_CRLID;
    PPOCSP_CRLID = ^POCSP_CRLID;
    OCSP_CRLID = record
        _crlUrl: PASN1_IA5STRING;
        _crlNum: PASN1_INTEGER;
        _crlTime: PASN1_GENERALIZEDTIME;
    end; { record }

    POCSP_SERVICELOC = ^OCSP_SERVICELOC;
    PPOCSP_SERVICELOC = ^POCSP_SERVICELOC;
    OCSP_SERVICELOC = record
        _issuer: PX509_NAME;
        _locator: PSTACK_OF_ACCESS_DESCRIPTION;
    end; { record }

    POCSP_REQ_CTX  = ^OCSP_REQ_CTX ;
    PPOCSP_REQ_CTX  = ^POCSP_REQ_CTX ;
    OCSP_REQ_CTX = record
      _state: TC_INT;
      _iobuf: PAnsiChar;
      _iobuflen: TC_INT;
      _io: PBIO;
      _mem: PBIO;
      _asn1_len: TC_ULONG;
    end;


{$ENDREGION}

    PRC2_KEY = ^RC2_KEY;
    PPRC2_KEY = ^PRC2_KEY;
    RC2_KEY = record
        _data: array[0..63] of RC2_INT;
    end; { record }

    PRC4_KEY = ^RC4_KEY;
    PPRC4_KEY = ^PRC4_KEY;
    RC4_KEY = record
        _x, _y: RC4_INT;
        _data: array[0..255] of RC4_INT;
    end; { record }

    PRC5_KEY = ^RC5_KEY;
    PPRC5_KEY = ^PRC5_KEY;
    RC5_KEY = record
        _rounds: TC_INT;
        _data: array[0..(2*(RC5_16_ROUNDS+1))-1] of RC5_32_INT;
    end; { record }

    RC5_32_KEY = RC5_KEY;
    PRC5_32_KEY = ^RC5_32_KEY;
    PPRC5_32_KEY = ^PRC5_32_KEY;

    PRIPEMD160_CTX = ^RIPEMD160_CTX;
    PPRIPEMD160_CTX = ^PRIPEMD160_CTX;
    RIPEMD160_CTX = record
      _A,_B,_C,_D,_E: RIPEMD160_LONG;
      _Nl,_Nh: RIPEMD160_LONG;
      _data: array [0..RIPEMD160_LBLOCK-1] of RIPEMD160_LONG;
      _num: TC_UINT;
    end; { record }

    PSHA_CTX = ^SHA_CTX;
    PPSHA_CTX = ^PSHA_CTX;
    SHA_CTX = record
      _h0,_h1,_h2,_h3,_h4: SHA_LONG;
      _Nl,_Nh: SHA_LONG;
      _data: array[0..SHA_LBLOCK-1] of SHA_LONG;
      _num: TC_UINT;
    end; { record }

    PSHA256_CTX = ^SHA256_CTX;
    PPSHA256_CTX = ^PSHA256_CTX;
    SHA256_CTX = record
      _h: array[0..7] of SHA_LONG;
      _Nl,_Nh: SHA_LONG;
      _data: array[0..SHA_LBLOCK-1] of SHA_LONG;
      _num,_md_len: TC_UINT;
    end; { record }

    SHA512_CTX_union = record
    case Byte of
      0: (_d: array [0..SHA_LBLOCK - 1] of SHA_LONG64);
      1: (_p: array [0..SHA512_CBLOCK - 1] of AnsiChar);
    end; { record }

    PSHA512_CTX = ^SHA512_CTX;
    PPSHA512_CTX = ^PSHA512_CTX;
    SHA512_CTX = record
      _h: array[0..7] of SHA_LONG64;
      _Nl, _Nh: SHA_LONG64;
      _u: SHA512_CTX_union;
      _num,_md_len: TC_UINT;
    end; { record }

{$REGION 'SSL'}

  PPSSL = ^PSSL;
  PSSL_CIPHER = ^SSL_CIPHER;
  PPSSL_CIPHER = ^PSSL_CIPHER;

  PSSL3_ENC_METHOD = Pointer; //^SSL3_ENC_METHOD;
  PPSSL3_ENC_METHOD = ^PSSL3_ENC_METHOD;
  PSSL_SESSION = ^SSL_SESSION;
  PPSSL_SESSION = ^PSSL_SESSION;
  PSSL_METHOD = ^SSL_METHOD;
  PPSSL_METHOD = ^PSSL_METHOD;
  PSRP_CTX = ^SRP_CTX;
  PPSRP_CTX = ^PSRP_CTX;
  PSSL_COMP = ^SSL_COMP;
  PPSSL_COMP = ^PSSL_COMP;
  PSSL_CTX = ^SSL_CTX;
  PPSSL_CTX = ^PSSL_CTX;
  PCERT = pointer;
  PSSL3_BUF_FREELIST = Pointer;
  PSSL2_STATE = Pointer;
  PSSL3_STATE = Pointer;
  PDTLS1_STATE = Pointer;

  SSL_METHOD_CALLBACK_FN = procedure; cdecl;

  PSRTP_PROTECTION_PROFILE = ^SRTP_PROTECTION_PROFILE;
  PPSRTP_PROTECTION_PROFILE = ^PSRTP_PROTECTION_PROFILE;
  SRTP_PROTECTION_PROFILE = record
    _name: PAnsiChar;
    _id: TC_LONG;
  end; { record }

  PSTACK_OF_SSL_CIPHER = PSTACK_OF;
  PSTACK_OF_SRTP_PROTECTION_PROFILE = PSTACK_OF;
  PSTACK_OF_SSL_COMP = PSTACK_OF;
  PLHASH_OF_SSL_SESSION = PLHASH_OF;

  GEN_SESSION_CB = function(const _ssl: PSSL; _id: PAnsiChar; var _id_len: TC_UINT): TC_INT; cdecl;

  TLS_SESSION_TICKET_EXT_CB_FN = function(_s: PSSL; const _data: PAnsiChar; _len: TC_INT; _arg: Pointer): TC_INT; cdecl;
  TLS_SESSION_SECRET_CB_FN = function(_s: PSSL; _secret: Pointer; var _secret_len: TC_INT; _peer_ciphers: PSTACK_OF_SSL_CIPHER; _cipher: PPSSL_CIPHER; _arg: Pointer): TC_INT; cdecl;

  PTLS_SESSION_TICKET_EXT = ^TLS_SESSION_TICKET_EXT;
  PPTLS_SESSION_TICKET_EXT = ^PTLS_SESSION_TICKET_EXT;
  TLS_SESSION_TICKET_EXT = record
    _length: TC_USHORT;
    _data: Pointer;
  end; { record }


  SSL_CIPHER = record
    _valid: TC_INT;
    _name: PAnsiChar;
    _id: TC_ULONG;
    _algorithm_mkey: TC_ULONG;
    _algorithm_auth: TC_ULONG;
    _algorithm_enc: TC_ULONG;
    _algorithm_mac: TC_ULONG;
    _algorithm_ssl: TC_ULONG;
    _algo_strength: TC_ULONG;
    _algorithm2: TC_ULONG;
    _strength_bits: TC_INT;
    _alg_bits: TC_INT;
  end; { record }

  SSL_METHOD = record
    _version: TC_INT;
    ssl_new: function(_s: PSSL): TC_INT; cdecl;
    ssl_clear: procedure(_s: PSSL); cdecl;
    ssl_free: procedure(_s: PSSL); cdecl;
    ssl_accept: function(_s: PSSL): TC_INT; cdecl;
    ssl_connect: function(_s: PSSL): TC_INT; cdecl;
    ssl_read: function(_s: PSSL; _buf: Pointer;_len: TC_INT): TC_INT; cdecl;
    ssl_peek: function(_s: PSSL; _buf: Pointer; _len: TC_INT): TC_INT; cdecl;
    ssl_write: function(_s: PSSL; const _buf: Pointer; len: TC_INT): TC_INT; cdecl;
    ssl_shutdown: function(_s: PSSL): TC_INT; cdecl;
    ssl_renegotiate: function(_s: PSSL): TC_INT; cdecl;
    ssl_renegotiate_check: function(_s: PSSL): TC_INT; cdecl;
    ssl_get_message: function(_s: PSSL; _st1: TC_INT; _stn: TC_INT; _mt: TC_INT; _max: TC_LONG; var _ok: TC_INT): TC_INT; cdecl;
    ssl_read_bytes: function(_s: PSSL; _type: TC_INT; _buf: PAnsiChar; len: TC_INT; _peek: TC_INT): TC_INT; cdecl;
    ssl_write_bytes: function(_s: PSSL; _type: TC_INT; const _buf: Pointer; len: TC_INT): TC_INT; cdecl;
    ssl_dispatch_alert: function(_s: PSSL): TC_INT; cdecl;
    ssl_ctrl: function(_s: PSSL; _cmd: TC_INT; _larg: TC_LONG; _parg: Pointer): TC_LONG; cdecl;
    ssl_ctx_ctrl: function(_ctx: PSSL_CTX; _cmd: TC_INT; _larg: TC_LONG; _parg: Pointer): TC_LONG; cdecl;
    get_cipher_by_char: function(const _ptr: PAnsiChar): PSSL_CIPHER; cdecl;
    put_cipher_by_char: function(const _cipher: PSSL_CIPHER; _ptr: PAnsiChar): TC_INT; cdecl;
    ssl_pending: function(const _s: PSSL): TC_INT; cdecl;
    num_ciphers: function: TC_INT; cdecl;
    get_cipher: function(_ncipher: TC_INT): PSSL_CIPHER; cdecl;
    get_ssl_method: function(_version: TC_INT): PSSL_METHOD; cdecl;
    get_timeout: function: TC_LONG; cdecl;
    ssl3_enc: function: PSSL3_ENC_METHOD;  cdecl;
    ssl_version: function: TC_INT; cdecl;
    ssl_callback_ctrl: function(_s: PSSL; _cb_id: TC_INT; fp: SSL_METHOD_CALLBACK_FN): TC_LONG; cdecl;
    ssl_ctx_callback_ctrl: function(_s: PSSL_CTX; _cb_id: TC_INT; fp: SSL_METHOD_CALLBACK_FN): TC_LONG; cdecl;
  end; { record }


  SSL_SESSION = record
    _ssl_version: TC_INT;
    _key_arg_length: TC_UINT;
    _key_arg: array[0..SSL_MAX_KEY_ARG_LENGTH-1] of AnsiChar;
    _master_key_length: TC_INT;
    _master_key: array[0..SSL_MAX_MASTER_KEY_LENGTH - 1] of AnsiChar;
    _session_id_length: TC_UINT;
    _session_id: array[0..SSL_MAX_SSL_SESSION_ID_LENGTH - 1] of AnsiChar;
    _sid_ctx_length: TC_UINT;
    _sid_ctx: array[0..SSL_MAX_SID_CTX_LENGTH - 1] of AnsiChar;
    _krb5_client_princ_len: TC_UINT;
    _krb5_client_princ: array[0..SSL_MAX_KRB5_PRINCIPAL_LENGTH - 1] of AnsiChar;
    _psk_identity_hint: PAnsiChar;
    _psk_identity: PAnsiChar;
    _not_resumable: TC_INT;
    _peer: PX509;
    _verify_result: TC_LONG;
    _references: TC_INT;
    _timeout: TC_LONG;
    _time: TC_LONG;
    _compress_meth: TC_UINT;
    _cipher: PSSL_CIPHER;
    _cipher_id: TC_ULONG;
    _ciphers: PSTACK_OF_SSL_CIPHER;
    _ex_data: PCRYPTO_EX_DATA;
    _prev, _next: PSSL_SESSION;
    _tlsext_hostname: PAnsiChar;
    _tlsext_ecpointformatlist_length: TC_SIZE_T;
    _tlsext_ecpointformatlist: PAnsiChar;
    _tlsext_ellipticcurvelist_length: TC_SIZE_T;
    _tlsext_ellipticcurvelist: PAnsiChar;
    _tlsext_tick: PAnsiChar;
    _tlsext_ticklen: TC_SIZE_T;
    _tlsext_tick_lifetime_hint: TC_LONG;
    _srp_username: PAnsiChar;
  end; { record }

  SRP_CTX = record
    _SRP_cb_arg: Pointer;
    TLS_ext_srp_username_callback: function(_s: PSSL; var _i: TC_INT; _p: Pointer): TC_INT; cdecl;
    SRP_verify_param_callback: function(_s: PSSL; _p: Pointer): TC_INT; cdecl;
    SRP_give_srp_client_pwd_callback: function(_s: PSSL; _p: Pointer): PAnsiChar; cdecl;
    _login: PAnsiChar;
    _N,_g,_s,_B,_A: PBIGNUM;
    _aa,_bb,_v: PBIGNUM;
    _info: PAnsiChar;
    _strength: TC_INT;
    _srp_Mask: TC_LONG;
  end; { record }

  SSL_COMP = record
    _id: TC_INT;
    _name: PAnsiChar;
{$ifndef OPENSSL_NO_COMP}
    _method: PCOMP_METHOD;
{$else}
    _method: PAnsiChar;
{$endif}
  end; { record }

  SSL_CTX_STATS = record
    _sess_connect: TC_INT;
    _sess_connect_renegotiate: TC_INT;
    _sess_connect_good: TC_INT;
    _sess_accept: TC_INT;
    _sess_accept_renegotiate: TC_INT;
    _sess_accept_good: TC_INT;
    _sess_miss: TC_INT;
    _sess_timeout: TC_INT;
    _sess_cache_full: TC_INT;
    _sess_hit: TC_INT;
    _sess_cb_hit: TC_INT;
  end; { record }

  SSL_CTX = record
    _method: PSSL_METHOD;
    _cipher_list: PSTACK_OF_SSL_CIPHER;
    _cipher_list_by_id: PSTACK_OF_SSL_CIPHER;
    _cert_store: PX509_STORE ;
    _sessions: PLHASH_OF_SSL_SESSION;
    _session_cache_size: TC_ULONG;
    _session_cache_head: PSSL_SESSION;
    _session_cache_tail: PSSL_SESSION;
    _session_cache_mode: TC_INT;
    _session_timeout: TC_LONG;
    new_session_cb: function(_ssl: PSSL; _sess: PSSL_SESSION): TC_INT; cdecl;
    remove_session_cb: procedure(_ctx: PSSL; _sess: PSSL_SESSION); cdecl;
    get_session_cb: function(_ssl: PSSL; _data: PAnsiChar; _len: TC_INT; var _copy: TC_INT): PSSL_SESSION; cdecl;
    _stats: SSL_CTX_STATS;
    _references: TC_INT;
    app_verify_callback: function(_ctx: PX509_STORE_CTX; _p: Pointer): TC_INT; cdecl;
    app_verify_arg: function: Pointer; cdecl;
    _default_passwd_callback: pem_password_cb;
    _default_passwd_callback_userdata: Pointer;
    client_cert_cb: function(_ssl: PSSL; _x509: PPX509; _pkey: PPEVP_PKEY): TC_INT; cdecl;
    app_gen_cookie_cb: function(_ssl: PSSL; _cookie: PAnsiChar; var _cookie_len: TC_INT): TC_INT; cdecl;
    app_verify_cookie_cb: function(_ssl: PSSL; _cookie: PAnsiChar; _cookie_len: TC_UINT): TC_INT; cdecl;
    _ex_data: CRYPTO_EX_DATA;
    _rsa_md5: PEVP_MD;
    _md5: PEVP_MD;
    _sha1: PEVP_MD;
    _extra_certs: PSTACK_OF_X509;
    _comp_methods: PSTACK_OF_SSL_COMP;
    info_callback: procedure(const _ssl: PSSL; _type: TC_INT; _val: TC_INT);  cdecl;
    _client_CA: PSTACK_OF_X509_NAME;
    _options: TC_ULONG;
    _mode: TC_ULONG;
    _max_cert_list: TC_LONG;
    _cert: PCERT;
    _read_ahead: TC_INT;
    msg_callback: procedure(_write_p: TC_INT; _version: TC_INT; _content_type: TC_INT; const _buf: Pointer; _len: TC_SIZE_T; _ssl: PSSL;  _arg: Pointer); cdecl;
    _msg_callback_arg: Pointer;
    _verify_mode: TC_INT;
    _sid_ctx_length: TC_UINT;
    _sid_ctx: array[0..SSL_MAX_SID_CTX_LENGTH - 1] of AnsiChar;
    default_verify_callback: function(_ok: TC_INT; _ctx: PX509_STORE_CTX): TC_INT; cdecl;
    _generate_session_id: GEN_SESSION_CB;
    _param: PX509_VERIFY_PARAM;
    _purpose: TC_INT;
    _trust: TC_INT;
    _quiet_shutdown: TC_INT;
    _max_send_fragment: TC_UINT;
    _client_cert_engine: PENGINE;
    tlsext_servername_callback: function(_s: PSSL; _i: PC_INT; _p: Pointer): TC_INT; cdecl;
    _tlsext_servername_arg: Pointer;
    _tlsext_tick_key_name: array[0..15] of ansichar;
    _tlsext_tick_hmac_key: array[0..15] of ansichar;
    _tlsext_tick_aes_key: array[0..15] of ansichar;
    tlsext_ticket_key_cb: function(_ssl: PSSL; _name: PAnsiChar; _iv: PAnsiChar; _ectx: PEVP_CIPHER_CTX; _hctx: PHMAC_CTX; _enc: TC_INT): TC_INT; cdecl;
    tlsext_status_cb: function(_ssl: PSSL; _arg: Pointer): TC_INT; cdecl;
    _tlsext_status_arg: Pointer;
    tlsext_opaque_prf_input_callback: function(_s: PSSL; _peerinput: Pointer; _len: TC_SIZE_T; _arg: Pointer): TC_INT; cdecl;
    _tlsext_opaque_prf_input_callback_arg: Pointer;

    _psk_identity_hint: PAnsiChar;
    psk_client_callback: function(_ssl: PSSL; const _hint: PAnsiChar; _identity: PAnsiChar; _max_identity_len: TC_UINT; _psk: PAnsiChar; _max_psk_len: TC_UINT): TC_UINT; cdecl;
    psk_server_callback: function(_ssl: PSSL; const _identity: PAnsiChar; _psk: PAnsiChar; _max_psk_len: TC_UINT): TC_UINT; cdecl;

    _freelist_max_len: TC_UINT;
    _wbuf_freelist: PSSL3_BUF_FREELIST;
    _rbuf_freelist: PSSL3_BUF_FREELIST;
    _srp_ctx: SRP_CTX;
    next_protos_advertised_cb: function(_s: PSSL; const _buf: PPAnsiChar; _len: PC_UINT; _arg: PAnsiChar): TC_INT; cdecl;
    _next_protos_advertised_cb_arg: Pointer;
    next_proto_select_cb: function(_s: PSSL; _out: PPAnsiChar; _outlen: PAnsiChar; const _in: PAnsiChar; _inlen: TC_UINT; _arg: Pointer): TC_INT; cdecl;
    _next_proto_select_cb_arg: Pointer;
    _srtp_profiles: PSTACK_OF_SRTP_PROTECTION_PROFILE;
  end; { record }

  PKSSL_CTX = Pointer;

  SSL = record
    _version: TC_INT;
    _type: TC_INT;
    _method: PSSL_METHOD;
{$IFNDEF NO_BIO}
    _rbio: PBIO;
    _wbio: PBIO;
    _bbio: PBIO;
{$else}
    _rbio: PAnsiChar;
    _wbio: PAnsiChar;
    _bbio: PAnsiChar;
{$endif}
    _rwstate: TC_INT;
    _in_handshake: TC_INT;
    handshake_func: function(_s: PSSL): TC_INT; cdecl;
    _server: TC_INT;
    _new_session: TC_INT;
    _quiet_shutdown: TC_INT;
    _shutdown: TC_INT;
    _state: TC_INT;
    _rstate: TC_INT;
    _init_buf: PBUF_MEM;
    _init_msg: Pointer;
    _init_num: TC_INT;
    _init_off: TC_INT;
    _packet: PAnsiChar;
    _packet_length: TC_UINT;

    _s2: PSSL2_STATE;
    _s3: PSSL3_STATE;
    _d1: PDTLS1_STATE;
    _read_ahead: TC_INT;

    msg_callback: procedure(_write_p: TC_INT; _version: TC_INT; _content_type: TC_INT; const _buf: Pointer; _len: TC_SIZE_T; _ssl: PSSL; _arg: Pointer); cdecl;
    _msg_callback_arg: Pointer;

    _hit: TC_INT;

    _param: PX509_VERIFY_PARAM;

    _purpose: TC_INT;
    _trust: TC_INT;

    _cipher_list: PSTACK_OF_SSL_CIPHER;
    _cipher_list_by_id: PSTACK_OF_SSL_CIPHER;

    _mac_flags: TC_INT;
    _enc_read_ctx: PEVP_CIPHER_CTX;
    _read_hash: PEVP_MD_CTX;
{$ifndef OPENSSL_NO_COMP}
    _expand: PCOMP_CTX;
{$else}
    _expand: PAnsiChar;
{$endif}
    _enc_write_ctx: PEVP_CIPHER_CTX;
    _write_hash: PEVP_MD_CTX;
{$ifndef OPENSSL_NO_COMP}
    _compress: PCOMP_CTX;
{$else}
    _compress: PAnsiChar;
{$endif}

    _cert: PCERT;

    _sid_ctx_length: TC_INT;
    _sid_ctx: array[0..SSL_MAX_SID_CTX_LENGTH - 1] of ansichar;

    _session: PSSL_SESSION;
    _generate_session_id: GEN_SESSION_CB;

    _verify_mode: TC_INT;
    verify_callback: function(_ok: TC_INT; _ctx: PX509_STORE_CTX): TC_INT; cdecl;
    info_callback: procedure(const _ssl: PSSL; _type: TC_INT; _val: TC_INT); cdecl;

    _error: TC_INT;
    _error_code: TC_INT;

{$ifndef OPENSSL_NO_KRB5}
    _kssl_ctx: PKSSL_CTX;
{$endif}

{$ifndef OPENSSL_NO_PSK}
    psk_client_callback: function(_ssl: PSSL; const _hint: PAnsiChar; _identity: PAnsiChar; _max_identity_len: TC_UINT; _psk: PAnsiChar; _max_psk_len: TC_UINT): TC_UINT; cdecl;
    psk_server_callback: function(_ssl: PSSL; const _identity: PAnsiChar; _psk: PAnsiChar; _max_psk_len: TC_UINT): TC_INT; cdecl;
{$endif}

    _ctx: PSSL_CTX;
    _debug: TC_INT;

    _verify_result: TC_LONG;
    _ex_data: CRYPTO_EX_DATA;

    _client_CA: PSTACK_OF_X509_NAME;

    _references: TC_INT;
    _options: TC_ULONG;
    _mode: TC_ULONG;
    _max_cert_list: TC_LONG;
    _first_packet: TC_INT;
    _client_version: TC_INT;
    _max_send_fragment: TC_UINT;

{$ifndef OPENSSL_NO_TLSEXT}
    tlsext_debug_cb: procedure(_s: PSSL; _client_server: TC_INT; _type: TC_INT; _data: PAnsiChar; _len: TC_INT; _arg: Pointer); cdecl;
    _tlsext_debug_arg: Pointer;
    _tlsext_hostname: PAnsiChar;
    _servername_done: TC_INT;
    _tlsext_status_type: TC_INT;
    _tlsext_status_expected: TC_INT;
    _tlsext_ocsp_ids: PSTACK_OF_OCSP_RESPID;
    _tlsext_ocsp_exts: PX509_EXTENSIONS;
    _tlsext_ocsp_resp: PAnsiChar;
    _tlsext_ocsp_resplen: TC_INT;
    _tlsext_ticket_expected: TC_INT;
{$ifndef OPENSSL_NO_EC}
    _tlsext_ecpointformatlist_length: TC_SIZE_T;
    _tlsext_ecpointformatlist: PAnsiChar;
    _tlsext_ellipticcurvelist_length: TC_SIZE_T;
    _tlsext_ellipticcurvelist: PAnsiChar;
{$endif}

    _tlsext_opaque_prf_input: Pointer;
    _tlsext_opaque_prf_input_len: TC_SIZE_T;

    _tlsext_session_ticket: PTLS_SESSION_TICKET_EXT;

    _tls_session_ticket_ext_cb: TLS_SESSION_TICKET_EXT_CB_FN;
    _tls_session_ticket_ext_cb_arg: Pointer;

    _tls_session_secret_cb: TLS_SESSION_SECRET_CB_FN;
    _tls_session_secret_cb_arg: Pointer;

    _initial_ctx: PSSL_CTX;
{$endif}

{$ifndef OPENSSL_NO_NEXTPROTONEG}
     _next_proto_negotiated: PAnsiChar;
    _ext_proto_negotiated_len: PAnsiChar;
{$endif}


    _srtp_profiles: PSTACK_OF_SRTP_PROTECTION_PROFILE;
    _srtp_profile: PSRTP_PROTECTION_PROFILE;

    _tlsext_heartbeat: TC_UINT;
    _tlsext_hb_pending: TC_UINT;
    _tlsext_hb_seq: TC_UINT;

  _renegotiate: TC_INT;

{$ifndef OPENSSL_NO_SRP}
    _srp_ctx: SRP_CTX;
{$endif}
  end; { record }



{$ENDREGION}

implementation

end.
