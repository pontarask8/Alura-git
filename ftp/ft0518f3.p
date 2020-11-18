/* Objetivo: Carregar informaá‰es das tabelas tempor†rias para impress∆o da NF-e baseado no retorno das TT's do programa axsep017/axsep037 */
{include/i-prgvrs.i FT0518F3 2.NFe310V2.00.025 }
DEFINE BUFFER alb-nota-fiscal FOR mgesp.alb-nota-fiscal.

&if "{&EMSFND_VERSION}" >= "1.00" &then
    {include/i-license-manager.i ft0518f MFT}
&endif

{cdp/cdcfgdis.i}

DEFINE TEMP-TABLE ttArquivo NO-UNDO
    FIELDS sequencia   AS INTEGER
    FIELDS nomeArquivo AS CHARACTER
     INDEX idx1 sequencia.

DEFINE INPUT PARAMETER c-impressora-so AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttArquivo.

DEFINE VARIABLE h-boal042ef                  AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-axsep037                   AS HANDLE      NO-UNDO.
DEFINE VARIABLE h-bcapi016                   AS HANDLE      NO-UNDO.
DEFINE VARIABLE iTpAmbSEFAZ                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-cod-uf-ibge                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-mult-nfe                   AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-count-nfe                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-soma-mod-nfe               AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-dig-ver-nfe                AS INTEGER     NO-UNDO.
DEFINE VARIABLE i-cont-itens                 AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-modelo-DANFE               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE c-chave-acesso-adicional-nfe AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-cont                       AS INTEGER     NO-UNDO.
DEFINE VARIABLE l-sem-Word                   AS LOGICAL     NO-UNDO.
DEFINE VARIABLE c-desc-mod-frete             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE i-cols-desc-item             AS INTEGER     NO-UNDO.
DEFINE VARIABLE c-infCpl-prod-perig          AS CHARACTER   NO-UNDO.

DEFINE SHARED VARIABLE r-nota               AS ROWID.
DEFINE SHARED VARIABLE c-hr-saida           AS CHARACTER FORMAT "xx:xx:xx" INITIAL "000000".
DEFINE SHARED VARIABLE l-dt                 AS LOGICAL FORMAT "Sim/Nao"  INITIAL NO.
DEFINE SHARED VARIABLE c-nr-nota-xml        AS CHARACTER.
DEFINE SHARED VARIABLE c-chave-xml          AS CHARACTER.
DEFINE SHARED VARIABLE l-gera-danfe-xml     AS LOGICAL.
DEFINE SHARED VARIABLE c-cod-dir-histor-xml AS CHARACTER.

DEFINE BUFFER bf-nota-fisc-adc-nfe FOR nota-fisc-adc.
DEFINE BUFFER unid-feder           FOR multi.unid-feder.

FIND FIRST param-global NO-LOCK NO-ERROR.
FIND FIRST nota-fiscal WHERE ROWID(nota-fiscal) = r-nota NO-LOCK NO-ERROR.

{ftp/ft0518f.i5} /* ttDanfe, ttDanfeItem */
{include/i-epc200.i ft0518f3} 
{adapters/xml/ep2/axsep037.i}

DEFINE TEMP-TABLE ttICMSDanfe NO-UNDO
    FIELDS orig         AS CHARACTER INITIAL ?                                         /* origem da mercadoria: 0 - Nacional 1 - Estrangeira - Importaá∆o direta 2 - Estrangeira - Adquirida no mercado interno */
    FIELDS CST          AS CHARACTER INITIAL ?                                         /* Tributá∆o pelo ICMS 00 - Tributada integralmente                                                                      */
    FIELDS vBC          AS DECIMAL   INITIAL ?  FORMAT ">>>>>>>>>>>>9.99" DECIMALS 2   /* Valor da BC do ICMS                                                                                                   */
    FIELDS vICMS        AS DECIMAL   INITIAL ?  FORMAT ">>>>>>>>>>>>9.99" DECIMALS 2   /* Valor do ICMS                                                                                                         */
    FIELDS pICMS        AS DECIMAL   INITIAL ?  FORMAT ">>9.99"           DECIMALS 2   /* Al°quota do ICMS                                                                                                      */
    FIELDS vBCST        AS DECIMAL   INITIAL ?  FORMAT ">>>>>>>>>>>>9.99" DECIMALS 2   /* Valor da BC do ICMS ST                                                                                                */
    FIELDS vICMSST      AS DECIMAL   INITIAL ?  FORMAT ">>>>>>>>>>>>9.99" DECIMALS 2   /* Valor do ICMS ST                                                                                                      */ 
    FIELDS CodEstabelNF AS CHARACTER INITIAL ?
    FIELDS SerieNF      AS CHARACTER INITIAL ?
    FIELDS NrNotaFisNF  AS CHARACTER INITIAL ?
    FIELDS ItCodigoNF   AS CHARACTER INITIAL ?
    FIELDS NrSeqFatNF   AS INTEGER   INITIAL ?
     INDEX ch-ttICMSDanfe CodEstabelNF SerieNF NrNotaFisNF NrSeqFatNF ItCodigoNF.

DEFINE TEMP-TABLE tt-doctos-impressos NO-UNDO
    FIELDS nr-nota-fis AS INTEGER
     INDEX idx-pu IS PRIMARY UNIQUE nr-nota-fis.

DEFINE TEMP-TABLE tt-data NO-UNDO 
    FIELDS dt-emis-nota LIKE nota-fiscal.dt-emis-nota
    FIELDS serie        LIKE nota-fiscal.serie
     INDEX idx001 IS PRIMARY UNIQUE dt-emis-nota serie.

DEFINE TEMP-TABLE tt-notas NO-UNDO
    FIELDS dt-emis-nota LIKE nota-fiscal.dt-emis-nota
    FIELDS serie        LIKE nota-fiscal.serie
    FIELDS nr-nota-fis  LIKE nota-fiscal.nr-nota-fis
     INDEX idx-001 IS PRIMARY UNIQUE nr-nota-fis.

{include/tt-edit.i}
{include/pi-edit.i}
{cdp/cdcfgdis.i}

DEFINE NEW SHARED VARIABLE i-num9100  AS INTEGER.
DEFINE NEW SHARED VARIABLE i-tam9100  AS INTEGER.
DEFINE NEW SHARED VARIABLE de-val9100 AS DECIMAL.
DEFINE NEW SHARED VARIABLE c-ext9100  AS CHARACTER EXTENT 10.

DEFINE VARIABLE c-desc-prod          LIKE ITEM.desc-item            NO-UNDO.
DEFINE VARIABLE w-peso-bruto-total   LIKE it-nota-fisc.peso-bruto   NO-UNDO.
DEFINE VARIABLE w-peso-liquido-total LIKE it-nota-fisc.peso-liq-fat NO-UNDO.
DEFINE VARIABLE de-vl-bipi-it        LIKE it-nota-fisc.vl-bipi-it   NO-UNDO. 
DEFINE VARIABLE de-vl-ipi-it         LIKE it-nota-fisc.vl-ipi-it    NO-UNDO. 
DEFINE VARIABLE de-ger-pisretido     LIKE nota-fiscal.vl-tot-nota   NO-UNDO. 
DEFINE VARIABLE de-ger-cofinsretido  LIKE nota-fiscal.vl-tot-nota   NO-UNDO. 
DEFINE VARIABLE de-ger-csllretido    LIKE nota-fiscal.vl-tot-nota   NO-UNDO. 
DEFINE VARIABLE de-vl-tot-item       LIKE it-nota-fisc.vl-tot-item  NO-UNDO. 
DEFINE VARIABLE de-vl-unit-trib        AS DECIMAL                   NO-UNDO FORMAT ">>>,>>>,>>9.99999".
DEFINE VARIABLE de-vl-unit             AS DECIMAL                   NO-UNDO FORMAT ">>>,>>>,>>9.99999".
DEFINE VARIABLE de-vl-total            AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE l-imprimiu-val         AS LOGICAL                   NO-UNDO.
DEFINE VARIABLE l-imprimiu-val-icm     AS LOGICAL                   NO-UNDO.
DEFINE VARIABLE c-pago                 AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE c-cgc                  AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE c-embal-desc           AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE c-especie              AS CHARACTER                 NO-UNDO.
DEFINE VARIABLE de-tot-qt-cilin        AS DECIMAL                   NO-UNDO.
DEFINE VARIABLE w-nota-dg              AS LOGICAL                   NO-UNDO.
DEFINE VARIABLE i-qt-volumes           AS INTEGER                   NO-UNDO.
DEFINE VARIABLE w-nro-item-vitalaire   AS INTEGER                   NO-UNDO.
DEFINE VARIABLE l-icms-outras-it       AS LOGICAL                   NO-UNDO. /* CD0606 - Considera ICMS Outras na NF-e */
DEFINE VARIABLE l-frete-bipi           AS LOGICAL                   NO-UNDO. 
DEFINE VARIABLE de-conv                AS DECIMAL                   NO-UNDO FORMAT ">>>>9.99" INITIAL 1.
DEFINE VARIABLE de-conv-pis            AS DECIMAL                   NO-UNDO FORMAT ">>>>9.99" INITIAL 1.
DEFINE VARIABLE de-conv-cofins         AS DECIMAL                   NO-UNDO FORMAT ">>>>9.99" INITIAL 1.
DEFINE VARIABLE de-conv-total          AS DECIMAL                   NO-UNDO FORMAT ">>>>9.99" INITIAL 1.

DEFINE BUFFER bf-det-rds                FOR det-rds. 
DEFINE BUFFER b-it-nota-fisc            FOR it-nota-fisc.
DEFINE BUFFER b2-it-nota-fisc           FOR it-nota-fisc.
DEFINE BUFFER b3-it-nota-fisc           FOR it-nota-fisc.
DEFINE BUFFER bf-alb-it-nota-fisc       FOR alb-it-nota-fisc.
DEFINE BUFFER bf_alb-it-nota-fisc_aux   FOR alb-it-nota-fisc.
DEFINE BUFFER bf-it-nota-fisc2          FOR it-nota-fisc.
DEFINE BUFFER bf-icms-estabelec         FOR estabelec.
DEFINE BUFFER b-nota-fiscal3            FOR nota-fiscal.
DEFINE BUFFER b-nota-fiscal2            FOR nota-fiscal.
DEFINE BUFFER b-nota-fiscal-compl       FOR nota-fiscal.
DEFINE BUFFER b-natur                   FOR natur-oper.
DEFINE BUFFER bf_update_alb-nota-fiscal FOR mgesp.alb-nota-fiscal.
DEFINE BUFFER bItemNotaFiscal           FOR it-nota-fisc.

IF l-gera-danfe-xml THEN DO:
    RUN ftp/ftapi552.p( INPUT c-cod-dir-histor-xml,
                        INPUT c-nr-nota-xml,
                        INPUT c-chave-xml,
                       OUTPUT TABLE ttAdi,
                       OUTPUT TABLE ttArma,
                       OUTPUT TABLE ttAutXML,
                       OUTPUT TABLE ttAvulsa,
                       OUTPUT TABLE ttCobr,
                       OUTPUT TABLE ttCOFINSAliq,
                       OUTPUT TABLE ttCOFINSNT,
                       OUTPUT TABLE ttCOFINSOutr,
                       OUTPUT TABLE ttCOFINSQtde,
                       OUTPUT TABLE ttCOFINSST,
                       OUTPUT TABLE ttICMSUFDest,
                       OUTPUT TABLE ttComb,
                       OUTPUT TABLE ttCompra,
                       OUTPUT TABLE ttDest,
                       OUTPUT TABLE ttDet,
                       OUTPUT TABLE ttDetExport,
                       OUTPUT TABLE ttDI,
                       OUTPUT TABLE ttDup,
                       OUTPUT TABLE ttEmit,
                       OUTPUT TABLE ttEntrega,
                       OUTPUT TABLE ttExporta,
                       OUTPUT TABLE ttICMS00,
                       OUTPUT TABLE ttICMS10,
                       OUTPUT TABLE ttICMS20,
                       OUTPUT TABLE ttICMS30,
                       OUTPUT TABLE ttICMS40,
                       OUTPUT TABLE ttICMS51,
                       OUTPUT TABLE ttICMS60,
                       OUTPUT TABLE ttICMS70,
                       OUTPUT TABLE ttICMS90,
                       OUTPUT TABLE ttICMSTot,
                       OUTPUT TABLE ttIde,
                       OUTPUT TABLE ttII,
                       OUTPUT TABLE ttImpostoDevol,
                       OUTPUT TABLE ttInfAdic,
                       OUTPUT TABLE ttIPI,
                       OUTPUT TABLE ttISSQN,
                       OUTPUT TABLE ttISSQNtot,
                       OUTPUT TABLE ttLacres,
                       OUTPUT TABLE ttMed,
                       OUTPUT TABLE ttNFe,
                       OUTPUT TABLE ttrefNF,
                       OUTPUT TABLE ttObsCont,
                       OUTPUT TABLE ttObsFisco,
                       OUTPUT TABLE ttPISAliq,
                       OUTPUT TABLE ttPISNT,
                       OUTPUT TABLE ttPISOutr,
                       OUTPUT TABLE ttPISQtde,
                       OUTPUT TABLE ttPISST,
                       OUTPUT TABLE ttProcRef,
                       OUTPUT TABLE ttReboque,
                       OUTPUT TABLE ttRetirada,
                       OUTPUT TABLE ttRetTrib,
                       OUTPUT TABLE ttTransp,
                       OUTPUT TABLE ttVeic,
                       OUTPUT TABLE ttVol,
                       OUTPUT TABLE ttrefNFP,
                       OUTPUT TABLE ttrefCTe,
                       OUTPUT TABLE ttrefECF,
                       OUTPUT TABLE ttICMSPart,
                       OUTPUT TABLE ttICMSST,
                       OUTPUT TABLE ttICMSSN101,
                       OUTPUT TABLE ttICMSSN102,
                       OUTPUT TABLE ttICMSSN201,
                       OUTPUT TABLE ttICMSSN202,
                       OUTPUT TABLE ttICMSSN500,
                       OUTPUT TABLE ttICMSSN900,
                       OUTPUT TABLE ttCana,
                       OUTPUT TABLE ttForDia,
                       OUTPUT TABLE ttDeduc,
                       OUTPUT TABLE ttRastro,
                       OUTPUT TABLE ttPag,
                       OUTPUT TABLE ttDetPag ).
END. ELSE DO:
    RUN adapters/xml/ep2/axsep037.p PERSISTENT SET h-axsep037.
    RUN pi-seta-nota-fiscal    IN h-axsep037 (INPUT r-nota).
    RUN pi-prepara-dados       IN h-axsep037.
    RUN pi-devolve-temp-tables IN h-axsep037 ( OUTPUT TABLE ttAdi,
                                               OUTPUT TABLE ttArma,
                                               OUTPUT TABLE ttAutXML,
                                               OUTPUT TABLE ttAvulsa,
                                               OUTPUT TABLE ttCobr,
                                               OUTPUT TABLE ttCOFINSAliq,
                                               OUTPUT TABLE ttCOFINSNT,
                                               OUTPUT TABLE ttCOFINSOutr,
                                               OUTPUT TABLE ttCOFINSQtde,
                                               OUTPUT TABLE ttCOFINSST,
                                               OUTPUT TABLE ttICMSUFDest,
                                               OUTPUT TABLE ttComb,
                                               OUTPUT TABLE ttCompra,
                                               OUTPUT TABLE ttDest,
                                               OUTPUT TABLE ttDet,
                                               OUTPUT TABLE ttDetExport,
                                               OUTPUT TABLE ttDI,
                                               OUTPUT TABLE ttDup,
                                               OUTPUT TABLE ttEmit,
                                               OUTPUT TABLE ttEntrega,
                                               OUTPUT TABLE ttExporta,
                                               OUTPUT TABLE ttICMS00,
                                               OUTPUT TABLE ttICMS10,
                                               OUTPUT TABLE ttICMS20,
                                               OUTPUT TABLE ttICMS30,
                                               OUTPUT TABLE ttICMS40,
                                               OUTPUT TABLE ttICMS51,
                                               OUTPUT TABLE ttICMS60,
                                               OUTPUT TABLE ttICMS70,
                                               OUTPUT TABLE ttICMS90,
                                               OUTPUT TABLE ttICMSTot,
                                               OUTPUT TABLE ttIde,
                                               OUTPUT TABLE ttII,
                                               OUTPUT TABLE ttImpostoDevol,
                                               OUTPUT TABLE ttInfAdic,
                                               OUTPUT TABLE ttIPI,
                                               OUTPUT TABLE ttISSQN,
                                               OUTPUT TABLE ttISSQNtot,
                                               OUTPUT TABLE ttLacres,
                                               OUTPUT TABLE ttMed,
                                               OUTPUT TABLE ttNFe,
                                               OUTPUT TABLE ttrefNF,
                                               OUTPUT TABLE ttObsCont,
                                               OUTPUT TABLE ttObsFisco,
                                               OUTPUT TABLE ttPISAliq,
                                               OUTPUT TABLE ttPISNT,
                                               OUTPUT TABLE ttPISOutr,
                                               OUTPUT TABLE ttPISQtde,
                                               OUTPUT TABLE ttPISST,
                                               OUTPUT TABLE ttProcRef,
                                               OUTPUT TABLE ttReboque,
                                               OUTPUT TABLE ttRetirada,
                                               OUTPUT TABLE ttRetTrib,
                                               OUTPUT TABLE ttTransp,
                                               OUTPUT TABLE ttVeic,
                                               OUTPUT TABLE ttVol,
                                               OUTPUT TABLE ttrefNFP,
                                               OUTPUT TABLE ttrefCTe,
                                               OUTPUT TABLE ttrefECF,
                                               OUTPUT TABLE ttICMSPart,
                                               OUTPUT TABLE ttICMSST,
                                               OUTPUT TABLE ttICMSSN101,
                                               OUTPUT TABLE ttICMSSN102,
                                               OUTPUT TABLE ttICMSSN201,
                                               OUTPUT TABLE ttICMSSN202,
                                               OUTPUT TABLE ttICMSSN500,
                                               OUTPUT TABLE ttICMSSN900,
                                               OUTPUT TABLE ttCana,
                                               OUTPUT TABLE ttForDia,
                                               OUTPUT TABLE ttDeduc,
                                               OUTPUT TABLE ttRastro,
                                               OUTPUT TABLE ttPag,
                                               OUTPUT TABLE ttDetPag ).
    IF VALID-HANDLE(h-axsep037) THEN DO:
        DELETE PROCEDURE h-axsep037.
        ASSIGN h-axsep037 = ?.
    END.
END.

EMPTY TEMP-TABLE ttDanfe.
EMPTY TEMP-TABLE ttDanfeItem.

FIND FIRST ttNFe      NO-LOCK NO-ERROR.
FIND FIRST ttIde      NO-LOCK NO-ERROR.
FIND FIRST ttEmit     NO-LOCK NO-ERROR.
FIND FIRST ttDest     NO-LOCK NO-ERROR.
FIND FIRST ttICMSTot  NO-LOCK NO-ERROR.
FIND FIRST ttTransp   NO-LOCK NO-ERROR.
FIND FIRST ttISSQNTot NO-LOCK NO-ERROR.
FIND FIRST ttInfAdic  NO-LOCK NO-ERROR.

CREATE ttDanfe.

RUN bcp/bcapi016.p PERSISTENT SET h-bcapi016.
RUN generateCODE128C IN h-bcapi016 (TRIM(ttNFe.ChaveAcessoNFe),OUTPUT ttDanfe.BCCODE128-chave).

DELETE PROCEDURE h-bcapi016.
ASSIGN h-bcapi016 = ?.

ASSIGN iTpAmbSEFAZ = INT( &if '{&bf_dis_versao_ems}' >= '2.09':U &then nota-fiscal.idi-tip-emis-amb-sefaz &else SUBSTRING(nota-fiscal.char-2,200,1) &endif ) NO-ERROR.
IF NOT (iTpAmbSEFAZ > 0) THEN ASSIGN iTpAmbSEFAZ = INT(ttIde.tpAmb).

FOR FIRST ser-estab NO-LOCK
    WHERE ser-estab.cod-estabel = nota-fiscal.cod-estabel
      AND ser-estab.serie       = nota-fiscal.serie:
   ASSIGN c-modelo-DANFE        = ( &if "{&bf_dis_versao_ems}" < "2.07":U &then SUBSTRING(ser-estab.char-1, 4, 1) &else STRING(ser-estab.idi-format-emis-danfe) &endif) /* Modelo de DANFE: 1=Retrato 2=Paisagem */
          l-sem-Word            = ( &if "{&bf_dis_versao_ems}":U >= "2.08":U &then ser-estab.log-word-danfe &else substring(ser-estab.char-1,70,1) = "S":U &endif).
END.

FIND FIRST estabelec OF nota-fiscal NO-LOCK NO-ERROR.
IF NOT AVAILABLE estabelec THEN RETURN "nok":u.

FIND FIRST natur-oper OF nota-fiscal NO-LOCK NO-ERROR.
IF NOT AVAILABLE natur-oper THEN RETURN "nok":u.

FIND FIRST emitente WHERE emitente.nome-abrev = nota-fiscal.nome-ab-cli NO-LOCK NO-ERROR.
IF NOT AVAILABLE emitente THEN RETURN "nok":u.

FIND FIRST ext-emitente WHERE ext-emitente.cod-emitente = emitente.cod-emitente NO-LOCK NO-ERROR.
FIND FIRST cond-pagto WHERE cond-pagto.cod-cond-pag = nota-fiscal.cod-cond-pag NO-LOCK NO-ERROR.
FIND FIRST transporte WHERE transporte.nome-abrev = nota-fiscal.nome-transp NO-LOCK NO-ERROR.

IF CAN-FIND( FIRST bItemNotaFiscal NO-LOCK 
             WHERE bItemNotaFiscal.cod-estabel   = nota-fiscal.cod-estabel
               AND bItemNotaFiscal.serie         = nota-fiscal.serie
               AND bItemNotaFiscal.nr-nota-fis   = nota-fiscal.nr-nota-fis
               AND bItemNotaFiscal.aliquota-icm  = 0
               AND ( bItemNotaFiscal.cd-trib-icm = 3 OR 
                     bItemNotaFiscal.cd-trib-icm = 5 ) ) THEN ASSIGN ttICMSTot.vBC = 0. // Paliativo atÇ publicaá∆o da NT2019.001
	 
ASSIGN ttDanfe.chavedeacessonfe          = STRING(ttNFe.ChaveAcessoNFe,"9999 9999 9999 9999 9999 9999 9999 9999 9999 9999 9999")
       ttDanfe.sn                        = ttIde.tpNF
       ttDanfe.razaosocialempresa        = ttEmit.xNome
       ttDanfe.enderecoemp               = (ttEmit.xLgr + " " + ttEmit.nro + " " + ttEmit.xCpl)
       ttDanfe.bairroemp                 = ttEmit.xBairro
       ttDanfe.cidadeemp                 = ttEmit.xMun
       ttDanfe.ufemp                     = ttEmit.UF
       ttDanfe.cepemp                    = STRING((IF ttEmit.CEP <> "" THEN FILL("0", 8 - LENGTH(STRING(ttEmit.CEP))) + STRING(ttEmit.CEP) ELSE STRING(ttEmit.CEP)), "99999-999" )
       ttDanfe.foneemp                   = ttEmit.fone
       ttDanfe.nrnota                    = ttNFe.NrNotaFisNF
       ttDanfe.ser                       = ttNFe.SerieNF
       ttDanfe.naturezaoperacao          = natur-oper.denominacao /* Comentado o fonte totvs ttIde.natOp */ 
       ttDanfe.inscrestadempresa         = ttEmit.IE
       ttDanfe.inscrestadsubstituto      = ttEmit.IEST
       ttDanfe.cnpjempresa               = (IF param-global.formato-id-federal <> "" THEN STRING(ttEmit.CNPJ, param-global.formato-id-federal) ELSE ttEmit.CNPJ)
       ttDanfe.cnpjdestinatario          = (IF ttDest.i-natureza = 1 /* Pessoa Fisica */ THEN IF param-global.formato-id-pessoal <> "" THEN STRING(ttDest.CPF,  param-global.formato-id-pessoal) ELSE ttDest.CPF ELSE IF param-global.formato-id-federal <> "" THEN STRING(ttDest.CNPJ, param-global.formato-id-federal) ELSE ttDest.CNPJ)
       ttDanfe.razaosocialdestinatario   = ttDest.xNome
       ttDanfe.dataemissao               = STRING(DATE(INTEGER(SUBSTR(ttIde.dhEmi,6,2)),INTEGER(SUBSTR(ttIde.dhEmi,9,2)),INTEGER(SUBSTR(ttIde.dhEmi,1,4))),"99/99/9999")
       ttDanfe.dataentrega               = (IF ttIde.dhSaiEnt <> ? AND TRIM(ttIde.dhSaiEnt) <> "" THEN STRING(DATE(INTEGER(SUBSTR(ttIde.dhSaiEnt,6,2)),INTEGER(SUBSTR(ttIde.dhSaiEnt,9,2)),INTEGER(SUBSTR(ttIde.dhSaiEnt,1,4)))) ELSE "")
       ttDanfe.horasaida                 = (IF ttIde.dhSaiEnt <> ? THEN SUBSTR(ttIde.dhSaiEnt,12,8) ELSE "")
       ttDanfe.enderecodestinatario      = (ttDest.xLgr + " " + ttDest.nro + " " + ttDest.xCpl)
       ttDanfe.cidadedestinatario        = ttDest.xMun
       ttDanfe.bairrodestinatario        = ttDest.xBairro
       ttDanfe.cepdestinatario           = STRING((IF ttDest.CEP <> "" THEN FILL("0", 8 - LENGTH(STRING(ttDest.CEP))) + STRING(ttDest.CEP) ELSE STRING(ttDest.CEP)),"99999-999")
       ttDanfe.fonedestinatario          = ttDest.fone
       ttDanfe.ufdest                    = ttDest.UF
       ttDanfe.inscrestaddestinatario    = ttDest.IE
       ttDanfe.vlbcicmsnota              = TRIM(STRING(ttICMSTot.vBC    ,"->>>,>>>,>>>,>>9.99"))
       ttDanfe.vlicmsnota                = TRIM(STRING(ttICMSTot.vICMS  ,"->>>,>>>,>>>,>>9.99"))
       ttDanfe.vlbcicmsstnota            = TRIM(STRING(ttICMSTot.vBCST  ,"->>>,>>>,>>>,>>9.99"))
       ttDanfe.vlicmsstnota              = TRIM(STRING(ttICMSTot.vST    ,"->>>,>>>,>>>,>>9.99"))
       ttDanfe.vltotprod                 = TRIM(STRING(ttICMSTot.vProd  ,"->>>,>>>,>>>,>>9.99"))
       ttDanfe.vlfretenota               = TRIM(STRING(ttICMSTot.vFrete ,"->>>,>>>,>>>,>>9.99"))
       ttDanfe.vlseguronota              = TRIM(STRING(ttICMSTot.vSeg   ,"->>>,>>>,>>>,>>9.99"))
       ttDanfe.vldescontonota            = IF AVAILABLE ext-emitente AND ext-emitente.cod-tipo = 7 THEN "0.00" ELSE TRIM(STRING(ttICMSTot.vDesc ,"->>>,>>>,>>>,>>9.99")) /* 30.04.2013 - chamado 65237 */
       ttDanfe.vldespesasnota            = TRIM(STRING(nota-fiscal.vl-embalagem,"->>>,>>>,>>>,>>9.99"))
       ttDanfe.vlipinota                 = TRIM(STRING(ttICMSTot.vIPI   ,"->>>,>>>,>>>,>>9.99"))
       ttDanfe.vltotnota                 = TRIM(STRING(ttICMSTot.vNF    ,"->>>,>>>,>>>,>>9.99"))
       ttDanfe.nometransp                = ttTransp.xNome
       ttDanfe.codantt1                  = ttTransp.RNTC
       ttDanfe.placa1                    = nota-fiscal.placa
       ttDanfe.ufpl1                     = nota-fiscal.uf-placa
       ttDanfe.inscrmunicipaliss         = estabelec.ins-municipal
       ttDanfe.vltotalsevicos            = IF AVAILABLE ttISSQNTot THEN TRIM(STRING(ttISSQNTot.vServ,"->>>,>>>,>>>,>>9.99")) ELSE TRIM(STRING(0 ,"->>>,>>>,>>>,>>9.99")) 
       ttDanfe.vlbciss                   = IF AVAILABLE ttISSQNTot THEN TRIM(STRING(ttISSQNTot.vBC  ,"->>>,>>>,>>>,>>9.99")) ELSE TRIM(STRING(0 ,"->>>,>>>,>>>,>>9.99")) 
       ttDanfe.vlisstotal                = IF AVAILABLE ttISSQNTot THEN TRIM(STRING(ttISSQNTot.vISS ,"->>>,>>>,>>>,>>9.99")) ELSE TRIM(STRING(0 ,"->>>,>>>,>>>,>>9.99")) 
       ttDanfe.informacoescomplementares = (IF AVAILABLE ttInfAdic THEN ttInfAdic.infCpl ELSE "")
       ttDanfe.contingencia              = "" /* ttInfAdic.infAdFisco - TK 587113 / TK 594340 */
       ttDanfe.homologacao1              = (IF iTpAmbSEFAZ = 2 /* Homologacao */ THEN "SEM VALOR FISCAL":U ELSE "")
       ttDanfe.homologacao2              = (IF iTpAmbSEFAZ = 2 /*Homologacao*/   THEN "Nota Fiscal Eletrìnica Autorizada em Ambiente de HOMOLOGAÄ«O.":U ELSE "").
											
	   IF l-gera-danfe-xml THEN DO:
           ASSIGN ttDanfe.dataentrega = (IF (ttIde.dhSaiEnt <> ? AND ttIde.dhSaiEnt <> "") THEN STRING(DATE(INTEGER(SUBSTR(ttIde.dhSaiEnt,6,2)), INTEGER(SUBSTR(ttIde.dhSaiEnt,9,2)), INTEGER(SUBSTR(ttIde.dhSaiEnt,1,4))),"99/99/9999") ELSE "") 
				  ttDanfe.horasaida   = (IF (ttIde.dhSaiEnt <> ? AND ttIde.dhSaiEnt <> "") THEN SUBSTR(ttIde.dhSaiEnt,12,8) ELSE "").
	   END. ELSE DO:
           FIND FIRST bf-nota-fisc-adc-nfe NO-LOCK
				WHERE bf-nota-fisc-adc-nfe.cod-estab        = nota-fiscal.cod-estabel
				  AND bf-nota-fisc-adc-nfe.cod-serie        = nota-fiscal.serie
				  AND bf-nota-fisc-adc-nfe.cod-nota-fisc    = nota-fiscal.nr-nota-fis
				  AND bf-nota-fisc-adc-nfe.cdn-emitente     = nota-fiscal.cod-emitente
				  AND bf-nota-fisc-adc-nfe.cod-natur-operac = nota-fiscal.nat-operacao
				  AND bf-nota-fisc-adc-nfe.idi-tip-dado     = 20 
               NO-ERROR. /* CD4035 - "Informacoes Adcionais NF-e" */
           ASSIGN ttDanfe.dataentrega = (IF (AVAILABLE bf-nota-fisc-adc-nfe AND bf-nota-fisc-adc-nfe.dat-livre-1 <> ?) THEN STRING(bf-nota-fisc-adc-nfe.dat-livre-1,"99/99/9999") ELSE IF nota-fiscal.dt-saida <> ? THEN STRING(nota-fiscal.dt-saida,"99/99/9999") ELSE "")
				  ttDanfe.horasaida   = (IF (AVAILABLE bf-nota-fisc-adc-nfe AND bf-nota-fisc-adc-nfe.hra-saida <> "") THEN STRING(bf-nota-fisc-adc-nfe.hra-saida,"99:99:99") ELSE IF c-hr-saida <> "00:00:00" AND nota-fiscal.dt-saida <> ? THEN c-hr-saida ELSE  IF nota-fiscal.dt-saida <> ? THEN STRING(TIME,"HH:MM:SS") ELSE "").
	   END.

       FOR EACH ct-trib-item-nota-fisc FIELDS( cod-estab cod-serie cod-nota-fisc cod-tip-trib val-trib ) NO-LOCK /* TK 587113 / TK 594340 */
           WHERE ct-trib-item-nota-fisc.cod-estab     = nota-fiscal.cod-estabel
             AND ct-trib-item-nota-fisc.cod-serie     = nota-fiscal.serie
             AND ct-trib-item-nota-fisc.cod-nota-fisc = nota-fiscal.nr-nota-fis
             AND ct-trib-item-nota-fisc.cod-tip-trib  = 'FCP': ASSIGN ttDanfe.contingencia = "Valor FCP: " + STRING( ct-trib-item-nota-fisc.val-trib ).
       END. /* TK 587113 / TK 594340 */

       IF ( AVAILABLE transporte AND transporte.nome-abrev <> "Remetente" AND transporte.nome-abrev <> "Destinat†rio" AND transporte.nome-abrev <> "Destinatario" ) THEN DO:
           ASSIGN ttDanfe.cnpjtransp       = ( IF ttTransp.i-natureza = 1 THEN IF param-global.formato-id-pessoal <> "" THEN STRING(ttTransp.CPF,  param-global.formato-id-pessoal) ELSE ttTransp.CPF ELSE IF param-global.formato-id-federal <> "" THEN STRING(ttTransp.CNPJ, param-global.formato-id-federal) ELSE ttTransp.CNPJ )
                  ttDanfe.enderecotransp   = ttTransp.xEnder                                          
                  ttDanfe.cidadetransp     = ttTransp.xMun
                  ttDanfe.uftran           = ttTransp.UF
                  ttDanfe.inscrestadtransp = ttTransp.IE.
       END. ELSE 
           ASSIGN ttDanfe.cnpjtransp       = ""
                  ttDanfe.enderecotransp   = ""
                  ttDanfe.cidadetransp     = ""
                  ttDanfe.uftran           = ""
                  ttDanfe.inscrestadtransp = "".
       
ASSIGN ttDanfe.codestab = "Estab.: " + nota-fiscal.cod-estabel
       ttDanfe.cepemp   = string((if estabelec.cep <> 0 then fill("0", 8 - length(string(estabelec.cep))) + string(estabelec.cep) else string(estabelec.cep)),"99999-999") 
       ttDanfe.sn       = ( if natur-oper.tipo = 1 then "0" else "1" ).

RUN pi-observacoes.

IF CAN-FIND( FIRST modalid-frete WHERE modalid-frete.cod-modalid-frete = ttTransp.modfrete ) THEN DO:
    FOR FIRST modalid-frete NO-LOCK
        WHERE modalid-frete.cod-modalid-frete = TRIM(ttTransp.modfrete):
        ASSIGN c-desc-mod-frete = TRIM(modalid-frete.cod-modalid-frete) + " - ":U + TRIM(modalid-frete.des-modalid-frete).
    END.
END. ELSE DO:
    CASE ttTransp.modfrete:
        WHEN "0":U THEN ASSIGN c-desc-mod-frete = "0 Œ Emitente":U.
        WHEN "1":U THEN ASSIGN c-desc-mod-frete = "1 Œ Destinat†rio/Remetente":U.
        WHEN "2":U THEN ASSIGN c-desc-mod-frete = "2 Œ Terceiros":U.
        WHEN "9":U THEN ASSIGN c-desc-mod-frete = "9 Œ Sem Frete":U.
    END CASE.
END.
    
ASSIGN ttDanfe.idfr = c-desc-mod-frete.

RUN pi-transportadora.

ASSIGN ttDanfe.especievolume = "".

FOR EACH ttVol NO-LOCK
   BREAK BY ttVol.siglaEmb:

    ACCUMULATE INT(ttVol.qVol) (TOTAL).
    ACCUMULATE     ttVol.pesoB (TOTAL).
    ACCUMULATE     ttVol.pesoL (TOTAL).

    FIND FIRST ext-natur-oper NO-LOCK 
         WHERE ext-natur-oper.nat-operacao = nota-fiscal.nat-operacao 
           AND ext-natur-oper.id-complemento-imposto 
        NO-ERROR.
    IF AVAILABLE ext-natur-oper THEN NEXT.

    IF LAST (ttVol.siglaEmb) THEN
        ASSIGN ttDanfe.qtvolume         = TRIM(STRING(ACCUMULATE TOTAL INT(ttVol.qVol),"->>>,>>>,>>>,>>9.99"))
               ttDanfe.marcavolume      = ttVol.marca
               ttDanfe.numeracaovolume  = ttVol.nVol
               ttDanfe.pesobrutototal   = TRIM(STRING(ACCUMULATE TOTAL ttVol.pesoB,"->>>,>>>,>>>,>>9.999"))
               ttDanfe.pesoliquidototal = TRIM(STRING(ACCUMULATE TOTAL ttVol.pesoL,"->>>,>>>,>>>,>>9.999")).

    IF nota-fiscal.cod-estabel = "02" THEN ASSIGN ttDanfe.qtvolume = nota-fiscal.nr-volumes.

    ASSIGN c-embal-desc    = " "
           de-tot-qt-cilin = 0.
    
    RUN piRetornaEmbalagem ( ROWID(nota-fiscal), OUTPUT c-embal-desc, OUTPUT de-tot-qt-cilin ).

    IF nota-fiscal.cod-estabel = "02" THEN ASSIGN c-embal-desc = "".

    ASSIGN w-nota-dg = NO.

    FOR EACH it-nota-fisc OF nota-fiscal NO-LOCK:
        FIND FIRST est-prod-coml WHERE est-prod-coml.it-codigo = it-nota-fisc.it-codigo NO-LOCK NO-ERROR.
        IF AVAILABLE est-prod-coml THEN DO:
            FIND FIRST ext-produto-coml WHERE ext-produto-coml.cod-produto-coml = est-prod-coml.cod-produto-coml NO-LOCK NO-ERROR.
            IF AVAILABLE ext-produto-coml AND ext-produto-coml.tipo-distr <> 3 THEN ASSIGN w-nota-dg = YES.
        END.        
    END.

    FIND FIRST param-global NO-LOCK NO-ERROR.

    FIND FIRST ext-ser-estab NO-LOCK 
        WHERE ext-ser-estab.serie       = nota-fiscal.serie       
          AND ext-ser-estab.cod-estabel = nota-fiscal.cod-estabel 
        NO-ERROR.
    IF AVAILABLE ext-ser-estab AND ext-ser-estab.tipo-docto <> 19 AND ext-ser-estab.tipo-docto <> 20 AND ext-ser-estab.tipo-docto <> 21 THEN DO:
        FIND FIRST nota-embal NO-LOCK 
            WHERE nota-embal.cod-estabel = nota-fiscal.cod-estabel    
              AND nota-embal.serie       = nota-fiscal.serie    
              AND nota-embal.nr-nota-fis = nota-fiscal.nr-nota-fis 
            NO-ERROR.         
        IF AVAILABLE nota-embal THEN DO:
            IF w-nota-dg = YES THEN ASSIGN ttDanfe.qtvolume = TRIM(STRING(de-tot-qt-cilin,"->>>,>>>,>>>,>>9.99")).
                               ELSE ASSIGN ttDanfe.qtvolume = TRIM(STRING(nota-embal.qt-volumes,"->>>,>>>,>>>,>>9.99"))
                                           c-embal-desc     = nota-embal.desc-vol.
        END.

        IF ttDanfe.qtvolume <> "0,00" THEN ASSIGN ttDanfe.especievolume = c-embal-desc.
                                      ELSE ASSIGN ttDanfe.especievolume = "".
        
        ASSIGN ttDanfe.marcavolume = nota-fiscal.marca-volume.
    END. ELSE DO:
        IF nota-fiscal.nr-volumes <> "" AND INTEGER(nota-fiscal.nr-volumes) <> 0 THEN DO:
            ASSIGN ttDanfe.qtvolume      = nota-fiscal.nr-volumes 
                   ttDanfe.especievolume = SUBSTRING(nota-fiscal.char-2,300,40)
                   ttDanfe.marcavolume   = nota-fiscal.marca-volume.
        END. ELSE DO:
            IF w-nota-dg = YES THEN ASSIGN ttDanfe.qtvolume = TRIM(STRING(de-tot-qt-cilin,"->>>,>>>,>>>,>>9.99")).
                               ELSE ASSIGN ttDanfe.qtvolume = TRIM(STRING(i-qt-volumes,"->>>,>>>,>>>,>>9.99")).
            
            IF ttDanfe.qtvolume <> "0,00" THEN ASSIGN ttDanfe.especievolume = c-embal-desc.
                                          ELSE ASSIGN ttDanfe.especievolume = "".
            
            ASSIGN ttDanfe.marcavolume = nota-fiscal.marca-volume.
        END.
    END.

    if can-find( first ret-cil-dg
                 where ret-cil-dg.cod-estabel        = nota-fiscal.cod-estabel
                   and ret-cil-dg.serie-nota-entrada = nota-fiscal.serie
                   and ret-cil-dg.nr-nota-entrada    = int(nota-fiscal.nr-nota-fis) ) then ttDanfe.qtvolume = trim(string(de-tot-qt-cilin,"->>>,>>>,>>>,>>9.99")).
    
    find first alb-nota-fiscal of nota-fiscal no-lock no-error.

    if available alb-nota-fiscal and ( alb-nota-fiscal.programa = 'al0109' OR alb-nota-fiscal.programa = 'al0231' ) then do:

        find first nota-embal no-lock
             where nota-embal.cod-estabel = alb-nota-fiscal.cod-estabel    
             and   nota-embal.serie       = alb-nota-fiscal.serie    
             and   nota-embal.nr-nota-fis = alb-nota-fiscal.nr-nota-fis 
             no-error.
        if available nota-embal and nota-embal.log-2 = yes then do:
            assign c-especie    = "" 
                   i-qt-volumes = 0.

            for each nota-embal no-lock use-index ch-nota-emb 
               where nota-embal.cod-estabel = alb-nota-fiscal.cod-estabel   
                 and nota-embal.serie       = alb-nota-fiscal.serie    
                 and nota-embal.nr-nota-fis = alb-nota-fiscal.nr-nota-fis
               break by nota-embal.sigla-emb:

                if first( nota-embal.sigla-emb ) then c-especie = nota-embal.desc-vol.
                                                 else if first-of( nota-embal.sigla-emb ) then c-especie = c-especie + "/" + nota-embal.desc-vol.

                accumulate nota-embal.qt-volumes( total ).

                if last( nota-embal.sigla-emb ) then i-qt-volumes = ACCUMULATE total nota-embal.qt-volumes.
            end.

            assign ttDanfe.qtvolume = trim(string(i-qt-volumes,"->>>,>>>,>>>,>>9.99")). 

            if ttDanfe.qtvolume <> "0" then ttDanfe.especievolume = c-especie. 
                                       else ttDanfe.especievolume = "".
        end.
    end.
    
    if substring( nota-fiscal.nat-operacao, 1, 1 ) = '7' then do:
        find first nota-embal of nota-fiscal no-lock no-error.
        if AVAILABLE nota-embal then
            assign ttDanfe.qtvolume      = trim(string( nota-embal.qt-volumes, "->>>,>>>,>>>,>>9.99" ))
                   ttDanfe.especievolume = nota-embal.desc-vol
                   ttDanfe.marca         = "".
    end.

    find first ext-ser-estab no-lock 
         where ext-ser-estab.serie       = nota-fiscal.serie       
           and ext-ser-estab.cod-estabel = nota-fiscal.cod-estabel 
        no-error.
    IF AVAILABLE ext-ser-estab AND ext-ser-estab.tipo-docto = 19 OR ext-ser-estab.tipo-docto = 20 OR ext-ser-estab.tipo-docto = 21 THEN DO: /* nota do Vitalaire */
        
        assign w-nro-item-vitalaire = 0
               ttDanfe.qtvolume     = "".
        
        for each it-nota-fisc of nota-fiscal no-lock:
            assign w-nro-item-vitalaire = w-nro-item-vitalaire + 1.
        end.
        
        if nota-fiscal.nr-volumes <> "" and integer(nota-fiscal.nr-volumes) <> 0 then assign ttDanfe.qtvolume = nota-fiscal.nr-volumes.
        else do:
            for each it-nota-fisc of nota-fiscal no-lock
                     ,first item no-lock of it-nota-fisc:

                if substring(it-nota-fisc.it-codigo,1,1) <> "2" then do:
                    find first ext-fam-comerc where ext-fam-comerc.fm-cod-com = item.fm-cod-com no-lock no-error.
                    find first est-prod-coml where est-prod-coml.it-codigo = it-nota-fisc.it-codigo no-lock no-error.
                    find first ext-est-prod-coml of est-prod-coml no-lock no-error.
                    find first capac-cilindros where capac-cilindros.cod-capacidade = ext-est-prod-coml.cod-capacidade no-lock no-error.
                    if available capac-cilindros then do:
                        
                        if capac-cilindros.Capacidade <> 0 then do:
                            if ext-fam-comerc.cil-vazio = no then assign ttDanfe.qtvolume  = string(integer(ttDanfe.qtvolume) + ((it-nota-fisc.qt-faturada[1] / capac-cilindros.Capacidade))).
                                                             else assign ttDanfe.qtvolume  = string(w-nro-item-vitalaire).
                        end. else
                            assign ttDanfe.qtvolume  = string(integer(ttDanfe.qtvolume) + 1).
                    end. else
                        assign ttDanfe.qtvolume  = string(integer(ttDanfe.qtvolume) + 1).
                end. else
                    assign ttDanfe.qtvolume  = string(integer(ttDanfe.qtvolume) + ((it-nota-fisc.qt-faturada[1]))).
            end.
        end.
        
        if w-nro-item-vitalaire = 1 then assign ttDanfe.especievolume = "UNIDADE".
        if w-nro-item-vitalaire = 0 then assign ttDanfe.especievolume = "".
        if w-nro-item-vitalaire > 1 then assign ttDanfe.especievolume = "UNIDADES".

        find first alb-nota-fiscal of nota-fiscal no-lock no-error.
        if available alb-nota-fiscal and ( alb-nota-fiscal.programa = 'al0109' or alb-nota-fiscal.programa = 'al0231' ) then do:
    
            find first nota-embal no-lock
                 where nota-embal.cod-estabel = alb-nota-fiscal.cod-estabel    
                 and   nota-embal.serie       = alb-nota-fiscal.serie    
                 and   nota-embal.nr-nota-fis = alb-nota-fiscal.nr-nota-fis 
                 no-error.
            if available nota-embal and nota-embal.log-2 = yes then do:
    
                assign c-especie    = "" 
                       i-qt-volumes = 0.
    
                for each nota-embal no-lock use-index ch-nota-emb 
                   where nota-embal.cod-estabel = alb-nota-fiscal.cod-estabel   
                     and nota-embal.serie       = alb-nota-fiscal.serie    
                     and nota-embal.nr-nota-fis = alb-nota-fiscal.nr-nota-fis
                   break by nota-embal.sigla-emb:
    
                    if first( nota-embal.sigla-emb ) then c-especie = nota-embal.desc-vol.
                                                     else if first-of( nota-embal.sigla-emb ) then c-especie = c-especie + "/" + nota-embal.desc-vol.
    
                    accumulate nota-embal.qt-volumes( total ).
                    if last( nota-embal.sigla-emb ) then i-qt-volumes = ACCUMULATE total nota-embal.qt-volumes.
                end.
    
                assign ttDanfe.qtvolume = trim(string(i-qt-volumes,"->>>,>>>,>>>,>>9.99")). 
                
                if ttDanfe.qtvolume <> "0" then ttDanfe.especievolume = c-especie.
                                           else ttDanfe.especievolume = "".
            end.
        end.
    end.

    assign w-peso-bruto-total      = 0
           w-peso-liquido-total    = 0.
    
    for each b2-it-nota-fisc of nota-fiscal no-lock:
        assign w-peso-bruto-total   = w-peso-bruto-total    + b2-it-nota-fisc.peso-bruto   
               w-peso-liquido-total = w-peso-liquido-total  + b2-it-nota-fisc.peso-liq-fat .
    end.
    
    assign ttDanfe.numeracaovolume = nota-fiscal.nr-volumes.

    IF w-peso-bruto-total <> 0 AND w-peso-liquido-total <> 0 AND NOT nota-fiscal.nat-operacao BEGINS "3" THEN DO:
        assign ttDanfe.pesobrutototal   = trim(string(w-peso-bruto-total,"->>>,>>>,>>>,>>9.9999"))
               ttDanfe.pesoliquidototal = trim(string(w-peso-liquido-total,"->>>,>>>,>>>,>>9.9999")).
    end. else do:
        assign ttDanfe.pesobrutototal   = trim(string(nota-fiscal.peso-bru-tot,"->>>,>>>,>>>,>>9.999"))
               ttDanfe.pesoliquidototal = trim(string(nota-fiscal.peso-liq-tot,"->>>,>>>,>>>,>>9.999")).
    end.
           
    IF SUBSTRING( nota-fiscal.nat-operacao, 1, 1 ) = '3' then do:

        FIND FIRST nota-embal OF nota-fiscal NO-LOCK NO-ERROR.
        IF AVAILABLE nota-embal THEN DO:
            ASSIGN ttDanfe.qtvolume        = TRIM(STRING(nota-embal.qt-volumes, "->>>,>>>,>>>,>>9.99"))
                   ttDanfe.especievolume   = nota-embal.sigla-emb
                   ttDanfe.marca           = nota-embal.desc-vol
                   ttDanfe.numeracaovolume = FILL(" ",20) + ttVol.nVol.

            FIND FIRST embalag WHERE embalag.sigla-emb = nota-embal.sigla-emb NO-LOCK NO-ERROR.
            IF AVAILABLE embalag THEN
                ASSIGN ttDanfe.especievolume = embalag.descricao.
        END.
    END.
    
    IF nota-fiscal.cod-estabel = "02" AND ttDanfe.especievolume  MATCHES ("*-*") THEN DO:
        ASSIGN ttDanfe.qtvolume      = ""
               ttDanfe.especievolume = ""
               ttDanfe.marcavolume   = "".
    END.

    find first bf_update_alb-nota-fiscal of nota-fiscal no-error.
    if AVAILABLE bf_update_alb-nota-fiscal then
        assign bf_update_alb-nota-fiscal.qtvolumes      = ttDanfe.qtvolume
               bf_update_alb-nota-fiscal.especievolumes = ttDanfe.especievolume.
    
    release bf_update_alb-nota-fiscal no-error.
END.

ASSIGN ttDanfe.protocoloautorizacao = &if "{&bf_dis_versao_ems}":U >= "2.07":U &then RIGHT-TRIM(nota-fiscal.cod-protoc) &else RIGHT-TRIM(SUBSTRING(nota-fiscal.char-1,97,15)) &endif.

FOR EACH ret-nf-eletro NO-LOCK
   WHERE ret-nf-eletro.cod-estabel = nota-fiscal.cod-estabel
     AND ret-nf-eletro.cod-serie   = nota-fiscal.serie      
     AND ret-nf-eletro.nr-nota-fis = nota-fiscal.nr-nota-fis
      BY ret-nf-eletro.dat-ret DESC
      BY ret-nf-eletro.hra-ret DESC:

    IF &if "{&bf_dis_versao_ems}" >= "2.07":U &then ret-nf-eletro.cod-protoc &else ret-nf-eletro.cod-livre-1 &endif = TRIM(ttDanfe.protocoloautorizacao) THEN DO:
        ASSIGN ttDanfe.protocoloautorizacao = ttDanfe.protocoloautorizacao + 
                                              "   " + STRING(ret-nf-eletro.dat-ret,"99/99/9999")  + /*DATA*/
                                              "   " + STRING(ret-nf-eletro.hra-ret,"xx:xx:xx").     /*HORA*/
        LEAVE.
    END.
END.
    
IF ttIde.tpEmis = "2" OR ttIde.tpEmis = "5" THEN DO:
    IF ttDest.xPais = "Brasil":U THEN DO:
        FOR FIRST unid-feder NO-LOCK
            WHERE unid-feder.pais   = ttDest.xPais
              AND unid-feder.estado = ttDest.UF:

            ASSIGN c-cod-uf-ibge = (&if  "{&bf_dis_versao_ems}"  >=  "2.07":U &then STRING(unid-feder.cod-uf-ibge) &else STRING(subSTRING(unid-feder.char-1,1,2)) &endif).
        END.
    END. ELSE 
        ASSIGN c-cod-uf-ibge = "99".
    
    ASSIGN c-chave-acesso-adicional-nfe = /* cUF      */ STRING(INT(c-cod-uf-ibge), '99') + 
                                          /* tpEmis   */ ttIde.tpEmis +
                                          /* CNPJ/CPF */ STRING(DEC(TRIM(REPLACE(REPLACE(REPLACE(ttDanfe.cnpjdestinatario,".",""),"-",""),"/",""))), '99999999999999') +
                                          /* vNF      */ STRING(ttICMSTot.vNF * 100, '99999999999999') + 
                                          /* ICMSp    */ (IF ttICMSTot.vICMS > 0 THEN "1" ELSE "2") +
                                          /* ICMSs    */ (IF ttICMSTot.vST > 0 THEN "1" ELSE "2") +  
                                          /* DD       */ STRING(SUBSTR(ttIde.dhEmi,9,2)).

    ASSIGN i-mult-nfe = 2.

    DO i-count-nfe = LENGTH(c-chave-acesso-adicional-nfe) TO 1 BY -1:
        ASSIGN i-soma-mod-nfe = i-soma-mod-nfe + (INT(SUBSTRING(c-chave-acesso-adicional-nfe,i-count-nfe,1)) * i-mult-nfe).
        ASSIGN i-mult-nfe = i-mult-nfe + 1.
        IF i-mult-nfe = 10 THEN ASSIGN i-mult-nfe = 2.
    END.
    
    IF i-soma-mod-nfe MODULO 11 = 0 OR
       i-soma-mod-nfe MODULO 11 = 1 THEN ASSIGN i-dig-ver-nfe = 0.
                                    ELSE ASSIGN i-dig-ver-nfe = 11 - (i-soma-mod-nfe MODULO 11).
        
    ASSIGN c-chave-acesso-adicional-nfe = c-chave-acesso-adicional-nfe + STRING(i-dig-ver-nfe). /* DV */

    RUN bcp/bcapi016.p PERSISTENT SET h-bcapi016.
    RUN generateCODE128C IN h-bcapi016 (INPUT TRIM(c-chave-acesso-adicional-nfe),OUTPUT ttDanfe.BCCODE128-chaveadicional).

    ASSIGN ttDanfe.chavedeacessoadicionalnfe = STRING(c-chave-acesso-adicional-nfe, "9999 9999 9999 9999 9999 9999 9999 9999 9999").

    DELETE PROCEDURE h-bcapi016.
    ASSIGN h-bcapi016 = ?.
END. ELSE DO:
    ASSIGN c-chave-acesso-adicional-nfe      = ""
           ttDanfe.chavedeacessoadicionalnfe = "".
END.

ASSIGN i-cont = 1.
FOR EACH ttDup NO-LOCK:
    IF i-cont = 9 THEN LEAVE. /*Danfe Padr∆o somente com 8 duplicatas. Se houverem mais, sair e nao imprimir. No XML ir∆o todas as fat-duplic existentes*/

    IF i-cont = 1 THEN
       ASSIGN ttDanfe.fatura1  = ttDup.nDup
              ttDanfe.vencfat1 = STRING(ttDup.dVenc,"99/99/9999")
              ttDanfe.vlfat1   = TRIM(STRING(ttDup.vDup,"->>>>,>>>,>>>,>>9.99")).

    IF i-cont = 2 THEN
       ASSIGN ttDanfe.fatura2  = ttDup.nDup                                                 
              ttDanfe.vencfat2 = STRING(ttDup.dVenc,"99/99/9999")                           
              ttDanfe.vlfat2   = TRIM(STRING(ttDup.vDup,"->>>>,>>>,>>>,>>9.99")).

    IF i-cont = 3 THEN
       ASSIGN ttDanfe.fatura3  = ttDup.nDup                                                 
              ttDanfe.vencfat3 = STRING(ttDup.dVenc,"99/99/9999")                           
              ttDanfe.vlfat3   = TRIM(STRING(ttDup.vDup,"->>>>,>>>,>>>,>>9.99")).

    IF i-cont = 4 THEN
       ASSIGN ttDanfe.fatura4  = ttDup.nDup                                                 
              ttDanfe.vencfat4 = STRING(ttDup.dVenc,"99/99/9999")                           
              ttDanfe.vlfat4   = TRIM(STRING(ttDup.vDup,"->>>>,>>>,>>>,>>9.99")).

    IF i-cont = 5 THEN
       ASSIGN ttDanfe.fatura5  = ttDup.nDup                                                 
              ttDanfe.vencfat5 = STRING(ttDup.dVenc,"99/99/9999")                           
              ttDanfe.vlfat5   = TRIM(STRING(ttDup.vDup,"->>>>,>>>,>>>,>>9.99")).

    IF i-cont = 6 THEN
       ASSIGN ttDanfe.fatura6  = ttDup.nDup                                                 
              ttDanfe.vencfat6 = STRING(ttDup.dVenc,"99/99/9999")                           
              ttDanfe.vlfat6   = TRIM(STRING(ttDup.vDup,"->>>>,>>>,>>>,>>9.99")).

    IF i-cont = 7 THEN
       ASSIGN ttDanfe.fatura7  = ttDup.nDup                                                 
              ttDanfe.vencfat7 = STRING(ttDup.dVenc,"99/99/9999")                           
              ttDanfe.vlfat7   = TRIM(STRING(ttDup.vDup,"->>>>,>>>,>>>,>>9.99")).

    IF i-cont = 8 THEN
       ASSIGN ttDanfe.fatura8  = ttDup.nDup                                                 
              ttDanfe.vencfat8 = STRING(ttDup.dVenc,"99/99/9999")                           
              ttDanfe.vlfat8   = TRIM(STRING(ttDup.vDup,"->>>>,>>>,>>>,>>9.99")).

    ASSIGN i-cont = i-cont + 1.
END.

IF ttIde.tpEmis = "1" OR ttIde.tpEmis = "3" OR ttIde.tpEmis = "6" OR ttIde.tpEmis = "7" THEN /* Tipo de Emiss∆o -> 1 Normal / 3 Contingància SCAN / 6 Contingància SVC-AN / 7 Contingància SVC-RS */
    ASSIGN ttDanfe.conteudovariavel1 = "www.nfe.fazenda.gov.br/portal ou no site da Sefaz Autorizadora":U
           ttDanfe.conteudovariavel2 = "PROTOCOLO DE AUTORIZAÄ«O DE USO":U.
ELSE IF ttIde.tpEmis = "4" THEN /* Tipo de Emiss∆o -> 4 - Contingància EPEC */
    ASSIGN ttDanfe.conteudovariavel1 = "www.nfe.fazenda.gov.br/portal":U
           ttDanfe.conteudovariavel2 = "NÈMERO DE REGISTRO EPEC":U.

ASSIGN i-cont-itens = 1.
FOR EACH ttDet NO-LOCK:
    CREATE ttDanfeItem.
    ASSIGN ttDanfeItem.iSeq      = i-cont-itens
           ttDanfeItem.cprod     = ttDet.cProd
           ttDanfeItem.descitem  = ttDet.xProd + ttDet.infAdProd
           ttDanfeItem.ncm       = ttDet.NCM
           ttDanfeItem.cfop      = ttDet.CFOP
           ttDanfeItem.u         = ttDet.uCom
           ttDanfeItem.quantitem = STRING(ttDet.qCom, "->>>,>>>,>>>,>>9.99<<":U).
    ASSIGN ttDanfeItem.vlunit    = STRING(ROUND(DEC(ttDet.vUnCom), fn-retorna-nro-decimais-nfe-FT0301()),">>>>>>>>>>>>>>9.99<<<<<<<<"). /*STRING(ttDet.vUnCom, "->,>>>,>>>,>>>,>>9.99<<<<<<<<":U)*/ /*COMENTADO - n∆o enviar sempre 10 decimais, e sim o que estiver parametrizado no FT0301*/

    FIND FIRST b3-it-nota-fisc NO-LOCK
        WHERE b3-it-nota-fisc.cod-estabel = ttDet.CodEstabelNF 
          AND b3-it-nota-fisc.serie       = ttDet.SerieNF      
          AND b3-it-nota-fisc.nr-nota-fis = ttDet.NrNotaFisNF  
          AND b3-it-nota-fisc.nr-seq-fat  = ttDet.NrSeqFatNF   
          AND b3-it-nota-fisc.it-codigo   = ttDet.ItCodigoNF   
        NO-ERROR.
    IF AVAILABLE b3-it-nota-fisc THEN DO:
        IF TRIM(SUBSTRING(b3-it-nota-fisc.char-2,298,10)) <> "" THEN
            ASSIGN ttDanfeItem.descitem = ttDanfeItem.descitem + " - CEST: " + TRIM(SUBSTRING(b3-it-nota-fisc.char-2,298,10)).
    END.
           
    ASSIGN ttDanfeItem.u-trib         = ttDet.uTrib
           ttDanfeItem.quantitem-trib = STRING(ttDet.qTrib, "->>>,>>>,>>>,>>9.99<<":U)
           ttDanfeItem.vlunit-trib    = STRING(ROUND(DEC(ttDet.vUnTrib),fn-retorna-nro-decimais-nfe-FT0301()),">>>>>>>>>>>>>>9.99<<<<<<<<") /*STRING(ttDet.vUnTrib, "->,>>>,>>>,>>>,>>9.99<<<<<<<<":U)*/ /*COMENTADO - n∆o enviar sempre 10 decimais, e sim o que estiver parametrizado no FT0301*/
           ttDanfeItem.vltotitem      = STRING(ttDet.vProd, "->>>,>>>,>>>,>>>,>>9.99":U).

    FOR EACH ttICMSDanfe:
        DELETE ttICMSDanfe.
    END.

    ASSIGN ttDanfeItem.vlbcicmit    = STRING(0, "->>>,>>>,>>>,>>9.99":U)
           ttDanfeItem.vlicmit      = STRING(0, "->>>,>>>,>>>,>>9.99":U)
           ttDanfeItem.icm          = STRING(0, "->>9.99":U)
           ttDanfeItem.vlbcicmit-st = STRING(0, "->>>,>>>,>>>,>>9.99":U)
           ttDanfeItem.vlicmit-st   = STRING(0, "->>>,>>>,>>>,>>9.99":U).

    FOR FIRST ttICMS40
        WHERE ttICMS40.CodEstabelNF = ttDet.CodEstabelNF
          AND ttICMS40.SerieNF      = ttDet.SerieNF     
          AND ttICMS40.NrNotaFisNF  = ttDet.NrNotaFisNF 
          AND ttICMS40.NrSeqFatNF   = ttDet.NrSeqFatNF
          AND ttICMS40.itcodigonf   = ttDet.ItCodigoNF
          AND ttICMS40.motDesICMS   = "7" :  /*Motivo da Desoneraá∆o do ICMS = 7 - SUFRAMA*/
          
        ASSIGN ttICMS40.vICMS = 0.    
    END.

    {ftp/ft0518f.i6 ttICMS00}
    {ftp/ft0518f.i6 ttICMS10}
    {ftp/ft0518f.i6 ttICMS20}
    {ftp/ft0518f.i6 ttICMS30}
    {ftp/ft0518f.i6 ttICMS40}
    {ftp/ft0518f.i6 ttICMS51}
    {ftp/ft0518f.i6 ttICMS60}
    {ftp/ft0518f.i6 ttICMS70}
    {ftp/ft0518f.i6 ttICMS90}
    {ftp/ft0518f.i9 ttICMSSN101} /* Simples nacional */
    {ftp/ft0518f.i9 ttICMSSN102} /* Simples nacional */
    {ftp/ft0518f.i9 ttICMSSN201} /* Simples nacional */
    {ftp/ft0518f.i9 ttICMSSN202} /* Simples nacional */
    {ftp/ft0518f.i9 ttICMSSN500} /* Simples nacional */
    {ftp/ft0518f.i9 ttICMSSN900} /* Simples nacional */

    ASSIGN ttDanfeItem.vlipiit = STRING(0, "->>>,>>>,>>>,>>9.99":U)
           ttDanfeItem.ipi     = STRING(0, "->>9.99":U).

    FOR FIRST ttIPI
        WHERE ttIPI.CodEstabelNF = ttDet.CodEstabelNF
          AND ttIPI.SerieNF      = ttDet.SerieNF     
          AND ttIPI.NrNotaFisNF  = ttDet.NrNotaFisNF 
          AND ttIPI.NrSeqFatNF   = ttDet.NrSeqFatNF
          AND ttIPI.ItCodigoNF   = ttDet.ItCodigoNF 
          AND ttIPI.l-ipi-trib   <> ?: /* quando l-ipi-trib = YES indica que tributa IPI */

        ASSIGN ttDanfeItem.vlipiit = STRING({ftp/ft0518f.i8 ttIPI.vIPI}, "->>>,>>>,>>>,>>9.99":U)
               ttDanfeItem.ipi     = STRING({ftp/ft0518f.i8 ttIPI.pIPI}, "->>9.99":U).

        FIND FIRST b3-it-nota-fisc NO-LOCK
            WHERE b3-it-nota-fisc.cod-estabel = ttDet.CodEstabelNF 
              AND b3-it-nota-fisc.serie       = ttDet.SerieNF      
              AND b3-it-nota-fisc.nr-nota-fis = ttDet.NrNotaFisNF  
              AND b3-it-nota-fisc.nr-seq-fat  = ttDet.NrSeqFatNF   
              AND b3-it-nota-fisc.it-codigo   = ttDet.ItCodigoNF    
            NO-ERROR.
        IF AVAILABLE b3-it-nota-fisc THEN DO:
            IF substring(b3-it-nota-fisc.nat-operacao,1,1) = "1" OR substring(b3-it-nota-fisc.nat-operacao,1,1) = "2" OR substring(b3-it-nota-fisc.nat-operacao,1,1) = "3" THEN DO:
                IF b3-it-nota-fisc.cd-trib-ipi = 3 THEN DO:
                    FIND FIRST b-natur WHERE b-natur.nat-operacao = b3-it-nota-fisc.nat-operacao NO-LOCK NO-ERROR.
                    IF SUBSTRING(natur-oper.char-1,151,1) = "N" THEN /*Parametro IMPRIME IPI OUTRAS NO DANFE - CD0606*/
                        ASSIGN ttDanfeItem.vlipiit = "0.00"
                               ttDanfeItem.ipi     = "0.00".
                END.
            END.            
        END.
        
    END.
    
    if dec(ttDanfeItem.ipi) = 0.00 then assign ttDanfeItem.ipi = "00".
    if dec(ttDanfeItem.icm) = 0.00 then assign ttDanfeItem.icm = "00".

    ASSIGN i-cont-itens = i-cont-itens + 1.
END.

DEFINE BUFFER bf-ttArquivo FOR ttArquivo.

FOR LAST bf-ttArquivo: END.
CREATE ttArquivo.
ASSIGN ttArquivo.sequencia   = IF AVAILABLE bf-ttArquivo THEN bf-ttArquivo.sequencia + 1 ELSE 1
       ttArquivo.nomeArquivo = TRIM(nota-fiscal.cod-estabel) + "-" + TRIM(nota-fiscal.serie) + "-" + TRIM(nota-fiscal.nr-nota-fis) + "-" + ".doc".

RUN ftp/ft0518f1.p( INPUT c-impressora-so,
                    INPUT TABLE ttDanfe,
                    INPUT TABLE ttDanfeItem,
                    INPUT ttArquivo.nomeArquivo,
                    INPUT c-modelo-Danfe,
                    INPUT l-sem-Word ).

RETURN "OK":U.

PROCEDURE piRetornaEmbalagem:
    DEFINE  INPUT PARAMETER ip-rw-nota-fiscal   AS ROWID             NO-UNDO.
    DEFINE OUTPUT PARAMETER op-embal-desc     LIKE embalag.descricao NO-UNDO.
    DEFINE OUTPUT PARAMETER op-tot-qt-cilin     AS DECIMAL           NO-UNDO.

    DEFINE BUFFER bf-nota-fiscal    FOR nota-fiscal.
    DEFINE BUFFER bf-it-nota-fisc   FOR it-nota-fisc.
    DEFINE BUFFER bf2-it-nota-fisc  FOR it-nota-fisc.
    DEFINE BUFFER balb-it-nota-fisc FOR alb-it-nota-fisc.
    
    find first bf-nota-fiscal no-lock 
         where rowid(bf-nota-fiscal) = ip-rw-nota-fiscal 
        no-error.
    if not available bf-nota-fiscal then
        assign op-embal-desc   = ""
               op-tot-qt-cilin = 0.
    else do:
        find first bf-it-nota-fisc of bf-nota-fiscal no-lock no-error.
        if available bf-it-nota-fisc then do:
            find first item no-lock where item.it-codigo = bf-it-nota-fisc.it-codigo no-error.
            find first item-caixa no-lock where item-caixa.it-codigo = bf-it-nota-fisc.it-codigo no-error.
            if available item-caixa then do:
                find first embalag no-lock where embalag.sigla-emb = item-caixa.sigla-emb no-error.
                if available embalag then do:
                    assign op-embal-desc = embalag.descricao.
                    if item-caixa.sigla-emb <> "SC" then
                        for each alb-it-nota-fisc of bf-nota-fiscal no-lock:
                            assign op-tot-qt-cilin = op-tot-qt-cilin + alb-it-nota-fisc.qtde-cilindros.
                        end.
                end. else do:
                    assign op-embal-desc   = " "                  
                           op-tot-qt-cilin = 0.  

                end.
            end. else do:
                find first item-caixa no-lock where item-caixa.fm-cod-com = item.fm-cod-com no-error.
                if available item-caixa then do:
                    find first embalag no-lock where embalag.sigla-emb = item-caixa.sigla-emb no-error.
                    if available embalag then do:
                        assign op-embal-desc = embalag.descricao.
                        if item-caixa.sigla-emb <> "SC" then 
                            for each alb-it-nota-fisc of bf-nota-fiscal no-lock:
                                assign op-tot-qt-cilin = op-tot-qt-cilin + alb-it-nota-fisc.qtde-cilindros.
                            end.
                    end. else do: 
                        assign op-embal-desc   = " "                  
                               op-tot-qt-cilin = 0.
                    end.
                end. 
                else do:

                    assign op-embal-desc = "".

                    for each bf2-it-nota-fisc of bf-nota-fiscal no-lock:  /* 06.07.2017 - Chamado 204932 - bf-it-nota-fisc - n∆o pode utilizar o mesmo buffer da pesquisa acima, pois faz o sistema se "perder", n∆o encontrando o registro do bf- abaixo*/

                        if w-nota-dg = yes then
                            assign op-tot-qt-cilin = op-tot-qt-cilin + bf2-it-nota-fisc.qt-faturada[1].     
                        else
                            assign op-tot-qt-cilin = 1.
                    end.
                end.
            end.
            
            if w-nota-dg = no and op-embal-desc = "" and bf-it-nota-fisc.un-fatur[1] = "M3" THEN /* 06.07.2017 - Chamdao 204932 - estava it-nota-fisc, o que gerava erro de item da nota n∆o encontrado */
                assign op-embal-desc  = "TANQUE".
        end.
        
        find first item-embal no-lock
             where item-embal.cod-estabel = bf-it-nota-fisc.cod-estabel
               and item-embal.serie       = bf-it-nota-fisc.serie
               and item-embal.nr-nota-fis = bf-it-nota-fisc.nr-nota-fis
               and item-embal.it-codigo   = bf-it-nota-fisc.it-codigo
               and item-embal.nr-seq-fat  = bf-it-nota-fisc.nr-seq-fat no-error.
        if available item-embal then do:

            find first embalag no-lock
                 where embalag.sigla-emb = item-caixa.sigla-emb no-error.
            if available embalag then do:

                assign op-embal-desc = embalag.descricao.

            end.
        end.
        
        if op-embal-desc = "" then do:
            find first nota-embal no-lock 
                 where nota-embal.cod-estabel = bf-nota-fiscal.cod-estabel 
                   and nota-embal.serie       = bf-nota-fiscal.serie  
                   and nota-embal.nr-nota-fis = bf-nota-fiscal.nr-nota-fis no-error.
            if available nota-embal then do:

                assign op-embal-desc = nota-embal.desc-vol.

                find first balb-it-nota-fisc no-lock 
                     where balb-it-nota-fisc.cod-estabel = bf-nota-fiscal.cod-estabel 
                     and   balb-it-nota-fisc.serie       = bf-nota-fiscal.serie 
                     and   balb-it-nota-fisc.nr-nota-fis = bf-nota-fiscal.nr-nota-fis no-error.

                if available balb-it-nota-fisc and balb-it-nota-fisc.tipo-item = 3 then  /* material */ 
                    assign op-tot-qt-cilin = nota-embal.qt-volumes.
            end.                
        end.

        IF op-tot-qt-cilin = 0 THEN
        DO:
            FOR FIRST ret-cil-dg NO-LOCK
                WHERE ret-cil-dg.cod-estabel        = bf-nota-fiscal.cod-estabel
                  AND ret-cil-dg.serie-nota-entrada = bf-nota-fiscal.serie
                  AND ret-cil-dg.nr-nota-entrada    = INTEGER(bf-nota-fiscal.nr-nota-fis):

                FOR EACH it-ordem-carreg NO-LOCK
                   WHERE it-ordem-carreg.cod-estabel = ret-cil-dg.cod-estabel
                     AND it-ordem-carreg.serie-rds   = ret-cil-dg.serie-rds
                     AND it-ordem-carreg.nr-rds      = ret-cil-dg.nr-rds
                     AND it-ordem-carreg.situacao    = 1:
    
                  ASSIGN 
                      op-embal-desc       = "CILINDROS"
                      ttDanfe.marcavolume = "ALB"
                      op-tot-qt-cilin     = op-tot-qt-cilin + it-ordem-carreg.qtde-cil-a1. 
                END.
            END.
        END.
    end.
END PROCEDURE.

procedure pi-observacoes:

    define variable c-nfs-canc            like nota-fiscal.nr-nota-fis extent 99   no-undo.
    define variable c-nfs                 like nota-fiscal.nr-nota-fis extent 99   no-undo.
    define variable c-nr-nota-ini         like tt-notas.nr-nota-fis                no-undo.
    define variable c-nr-nota-fim         like tt-notas.nr-nota-fis                no-undo.
    define variable c-serie               like tt-data.serie                       no-undo.
    define variable de-valor-icms         like nota-fiscal.vl-tot-nota             no-undo.
    define variable de-total-bas-icm      like it-nota-fisc.vl-icms-it             no-undo.
    define variable w-form-de             like mgesp.alb-nota-fiscal.form-de       no-undo.
    define variable w-form-ate            like mgesp.alb-nota-fiscal.form-ate      no-undo.
    define variable w-form2-de            like mgesp.alb-nota-fiscal.form2-de      no-undo.
    define variable w-form2-ate           like mgesp.alb-nota-fiscal.form2-ate     no-undo.
    define variable w-cod-coletor         like mgesp.alb-nota-fiscal.cod-coletor   no-undo.
    define variable w-serie-coletor       like mgesp.alb-nota-fiscal.serie-coletor no-undo.
    define variable c-nota                  as character format 'x(256)'           no-undo.
    define variable c-mensagem2             as character format "x(2000)"          no-undo. 
    define variable c-nota-canc             as character format 'x(256)'           no-undo.
    define variable c-linha-imp             as character format 'x(256)'           no-undo.
    define variable c-compl-carga           as character                           no-undo.
    define variable c-nf-origem             as character                           no-undo.
    define variable dt-data                 as date                                no-undo.
    define variable acum-valor-icms         as decimal                             no-undo.
    define variable i-cont                  as integer   initial 1                 no-undo.
    define variable i-aux                   as integer                             no-undo.
    define variable lComplementarDeEntrada  as logical                             no-undo.
    define variable l-nf-compl-carg         as logical                             no-undo.
    define variable l-desc-icms             as logical                             no-undo.
                                                                                 
    ASSIGN c-mensagem2 = ttDanfe.informacoescomplementares + " " + ( IF AVAILABLE ttInfAdic THEN ttInfAdic.infAdFisco ELSE "" ).
    
    if substring(nota-fiscal.nat-operacao,1,1) = "1" or 
       substring(nota-fiscal.nat-operacao,1,1) = "2" or
       substring(nota-fiscal.nat-operacao,1,1) = "3" then do:

        find first mensagem-estabelec no-lock 
             where mensagem-estabelec.cod-estabel  = nota-fiscal.cod-estabel  
             and   mensagem-estabelec.nat-operacao = nota-fiscal.nat-operacao no-error.
        if available mensagem-estabelec then do:
    
            find first mensagem no-lock 
                 where mensagem.cod-mensagem = mensagem-estabelec.cod-mensagem no-error.                                       
            IF available mensagem then  do:
                if not c-mensagem2 matches "*" + mensagem.texto-mensag + "*" then
                    assign c-mensagem2 = c-mensagem2 + " " + mensagem.texto-mensag.            
            end.
        end.
    end.

    for first it-nota-fisc of nota-fiscal no-lock:
    
        if nota-fiscal.cod-estabel <> "02" THEN do:
    
            for each b2-it-nota-fisc of nota-fiscal no-lock:
                
                find first b-nota-fiscal2 no-lock 
                     where b-nota-fiscal2.cod-estabel = b2-it-nota-fisc.cod-estabel 
                     and   b-nota-fiscal2.serie       = b2-it-nota-fisc.serie-docum 
                     and   b-nota-fiscal2.nr-nota-fis = b2-it-nota-fisc.nr-docum no-error.
                if available b-nota-fiscal2 and b-nota-fiscal2.cod-emitente = nota-fiscal.cod-emitente then do: 
                    if not c-mensagem2 matches "*Retorno ref*" then 
                        assign c-mensagem2 = c-mensagem2 + "  " +
                                             " Retorno ref. Nota : " + string(b2-it-nota-fisc.nr-docum,"x(16)") +  
                                             " SÇrie: " + string(b2-it-nota-fisc.serie-docum,"x(5)") +  
                                             " Data:  " + string(b-nota-fiscal2.dt-emis-nota,"99/99/9999") + 
                                             " Valor: " + string(b-nota-fiscal2.vl-tot-nota,">>>,>>>,>>9.99").
                    else
                        IF NOT c-mensagem2 MATCHES("*" + b2-it-nota-fisc.nr-docum + "*") THEN
                            assign c-mensagem2 = c-mensagem2 + "  " +
                                                 " Nota : " + string(b2-it-nota-fisc.nr-docum,"x(16)") +  
                                                 " SÇrie: " + string(b2-it-nota-fisc.serie-docum,"x(5)") +  
                                                 " Data:  " + string(b-nota-fiscal2.dt-emis-nota,"99/99/9999") + 
                                                 " Valor: " + string(b-nota-fiscal2.vl-tot-nota,">>>,>>>,>>9.99").
                end.
                else do:
                    
                    find first docum-est where
                        docum-est.serie-docto  = b2-it-nota-fisc.serie-docum    and
                        docum-est.nro-docto    = b2-it-nota-fisc.nr-docum       and
                        docum-est.cod-emitente = nota-fiscal.cod-emitente    and
                        docum-est.nat-operacao = b2-it-nota-fisc.nat-docum    no-lock no-error.
                    if available docum-est then do:
                        
                        if not c-mensagem2 matches "*Retorno ref*" then 
                            assign c-mensagem2 = c-mensagem2 + "  " +
                                                 " Retorno ref. Nota : " + string(b2-it-nota-fisc.nr-docum,"x(16)") +  
                                                 " SÇrie: " + string(b2-it-nota-fisc.serie-docum,"x(5)") +  
                                                 " Data:  " + string(docum-est.dt-emissao,"99/99/9999") + 
                                                 " Valor: " + string(docum-est.tot-valor,">>>,>>>,>>9.99").
                        else
                            IF NOT c-mensagem2 MATCHES("*" + b2-it-nota-fisc.nr-docum + "*") THEN
                                assign c-mensagem2 = c-mensagem2 + "  " +
                                                     " Nota : " + string(b2-it-nota-fisc.nr-docum,"x(16)") +  
                                                     " SÇrie: " + string(b2-it-nota-fisc.serie-docum,"x(5)") +  
                                                     " Data:  " + string(docum-est.dt-emissao,"99/99/9999") + 
                                                     " Valor: " + string(docum-est.tot-valor,">>>,>>>,>>9.99").
                        
                    end.
    
                end.
            end.
            
            if estabelec.estado = "MG" then do:
                
                find first ret-cil-dg no-lock 
                     where ret-cil-dg.est-nota-entrada     = nota-fiscal.cod-estabel 
                     and   ret-cil-dg.serie-nota-entrada   = nota-fiscal.serie       
                     and   ret-cil-dg.nr-nota-entrada      = integer(nota-fiscal.nr-nota-fis) no-error.
                if available ret-cil-dg then do:
                    
                    find first rds-dg OF ret-cil-dg no-lock no-error.
                    if available rds-dg then do:
                        
                        assign c-nfs      = ""
                               c-nfs-canc = "".
                        
                        for each det-rds-dg no-lock
                           where det-rds-dg.cod-estabel = rds-dg.cod-estabel 
                           and   det-rds-dg.serie-rds   = rds-dg.serie-rds 
                           and   det-rds-dg.nr-rds      = rds-dg.nr-rds:
                            
                            if det-rds-dg.nr-nota-saida = 0 then next.
                            
                            find first tt-doctos-impressos no-lock 
                                 where tt-doctos-impressos.nr-nota-fis = det-rds-dg.nr-nota-saida no-error.
                            if not available tt-doctos-impressos then do:
                                create tt-doctos-impressos.
                                assign tt-doctos-impressos.nr-nota-fis = det-rds-dg.nr-nota-saida.
                            end.
                            else next.
                            
                            assign i-aux = i-aux + 1.
                            
                            find first b-nota-fiscal2 NO-LOCK
                                 where b-nota-fiscal2.cod-estabel = det-rds-dg.cod-estabel 
                                 and   b-nota-fiscal2.serie       = det-rds-dg.serie-nota-saida 
                                 and   b-nota-fiscal2.nr-nota-fis = string(det-rds-dg.nr-nota-saida,'9999999') 
                                 and   b-nota-fiscal2.dt-cancela <> ? no-error.
                            if available b-nota-fiscal2 then 
                                assign c-nfs-canc[i-aux] = string(det-rds-dg.nr-nota-saida,'9999999').
                            else 
                                assign c-nfs[i-aux]      = string(det-rds-dg.nr-nota-saida,'9999999').
                            
                            assign c-serie = det-rds-dg.serie-nota-saida.
                        end.
                        
                        assign c-nota      = ""
                               c-nota-canc = ""
                               c-linha-imp = "".
                        
                        repeat i-cont = 1 to i-aux:
                            if c-nfs[i-cont] <> "" then do:
                                if i-cont = 1 then assign c-nota = c-nfs[i-cont].
                                else if i-cont > 1 then assign c-nota = c-nota + ", " + c-nfs[i-cont].
                            end.
                    
                            if c-nfs-canc[i-cont] <> "" then do:
                                if i-cont = 1 then assign c-nota-canc = c-nfs-canc[i-cont].
                                else if i-cont > 1 then assign c-nota-canc = c-nota-canc + " , " + c-nfs-canc[i-cont].
                            end.
                        end.
                        
                        if c-nota      <> "" then assign c-linha-imp = "NF de Venda nr.: " + trim(c-nota) + " Serie.: " + c-serie.
                        if c-nota-canc <> "" then assign c-linha-imp = c-linha-imp + " " + " NF Cancelada(s) nr.: " + trim(c-nota-canc).
                        
                        find first rota-dg of rds-dg no-lock no-error.
                        if available rota-dg and rota-dg.emissao-doc = 2 then do:
                            find first coletor of rds-dg no-lock no-error.
                            if available coletor then do:
                                if not c-mensagem2 matches "*Nr. Coletor*" then
                                    assign c-mensagem2 = c-mensagem2 + " " + 
                                                         c-linha-imp + " " +
                                                         " Nr. Coletor.: " + rds-dg.cod-coletor + 
                                                         " Nr. Ordem.: " + trim(string(coletor.serie)).
                            end.
                        end.
                    end.
                end. 
    
                find first det-rds no-lock
                     where det-rds.est-nota-entrada   = nota-fiscal.cod-estabel 
                     and   det-rds.serie-nota-entrada = nota-fiscal.serie 
                     and   det-rds.nr-nota-entrada    = integer(nota-fiscal.nr-nota-fis) no-error.
                if available det-rds then
                    find first rds of det-rds no-lock no-error.
                
                if available rds then do:
            
                    for each bf-det-rds of rds no-lock:
    
                        if bf-det-rds.nr-nota-saida = 0 then next.
                        assign i-aux = i-aux + 1.
                        
                        find first b-nota-fiscal2 no-lock
                             where b-nota-fiscal2.cod-estabel = bf-det-rds.cod-estabel 
                             and   b-nota-fiscal2.serie       = bf-det-rds.serie-nota-saida 
                             and   b-nota-fiscal2.nr-nota-fis = string(bf-det-rds.nr-nota-saida,'9999999') 
                             and   b-nota-fiscal2.dt-cancela <> ? no-error.
                        if available b-nota-fiscal2 then 
                            assign c-nfs-canc[i-aux] = string(bf-det-rds.nr-nota-saida,'9999999').
                        else 
                            assign c-nfs[i-aux]      = string(bf-det-rds.nr-nota-saida,'9999999').
                        
                        assign c-serie = bf-det-rds.serie-nota-saida.
                    end.
                    
                    assign c-nota      = ""
                           c-linha-imp = "".
                    
                    repeat i-cont = 1 to i-aux:
                        if c-nfs[i-cont] <> "" then do:
                            if i-cont = 1 then assign c-nota = c-nfs[i-cont].
                            else if i-cont > 1 then assign c-nota =  c-nota + ", " + c-nfs[i-cont].
                        end.
                
                        if c-nfs-canc[i-cont] <> "" then do:
                            if i-cont = 1 then assign c-nota-canc = c-nfs-canc[i-cont].
                            else if i-cont > 1 then assign c-nota-canc =  c-nota-canc + " , " + c-nfs-canc[i-cont].
                        end.
                    end.
                    
                    if c-nota      <> "" then assign c-linha-imp = "NF de Venda nr.: " + trim(c-nota) + " Serie.: " + c-serie.
                    if c-nota-canc <> "" then assign c-linha-imp = c-linha-imp + " NF Cancelada(s) nr.: " + trim(c-nota-canc).
                    
                    find first coletor of rds no-lock no-error.
                    
                    if not c-mensagem2 matches "*Nr. Coletor*" then
                        assign c-mensagem2 = c-mensagem2 + " " + 
                                             c-linha-imp + 
                                             " Nr. Coletor.: " + rds.cod-coletor + 
                                             " Nr. Ordem.: " + trim(string(coletor.serie)).
            
                end.
            end. 
        end. else do:
            IF it-nota-fisc.cod-estabel <> "02" THEN DO:
                if string(it-nota-fisc.nr-pedcli) <> "" then do:
                    if not c-mensagem2 matches "*" + "   PEDIDO: " + string(it-nota-fisc.nr-pedcli) + "*" then
                        assign c-mensagem2 = c-mensagem2 + "   PEDIDO: " + string(it-nota-fisc.nr-pedcli).
                end.
            END.

            find first ped-venda no-lock 
                 where ped-venda.nome-abrev = nota-fiscal.nome-ab-cli    
                 and   ped-venda.nr-pedcli  = nota-fiscal.nr-pedcli no-error.
            if available ped-venda THEN do:
                if not c-mensagem2 matches "*" + ped-venda.observacoes + "*" then
                    assign c-mensagem2 = c-mensagem2 + " " + ped-venda.observacoes.
            end.
        end.
    end.

    for first b-it-nota-fisc of nota-fiscal no-lock:
        find first alb-it-nota-fisc of b-it-nota-fisc no-lock no-error.
        if available alb-it-nota-fisc then do:
            find first pol-coml no-lock 
                 where pol-coml.nr-politica = alb-it-nota-fisc.nr-politica 
                no-error.
            if available pol-coml then do:
                if not c-mensagem2 matches "*" + pol-coml.observacoes + "*" then assign c-mensagem2 = c-mensagem2 + " " + pol-coml.observacoes.
            end.
        end.    
    end.
    
    assign lComplementarDeEntrada = no
           l-nf-compl-carg        = no.

    for each det-rds-dg NO-LOCK 
       where det-rds-dg.estab-compl-entrada = nota-fiscal.cod-estabel 
         and det-rds-dg.serie-compl-entrada = nota-fiscal.serie 
         and det-rds-dg.nr-compl-entrada    = integer(nota-fiscal.nr-nota-fis) 
         and det-rds-dg.situacao            = 1:
        
        assign lComplementarDeEntrada = yes.
        
        find first b-nota-fiscal3 no-lock
             where b-nota-fiscal3.cod-estabel = det-rds-dg.estab-nota-carga 
             and   b-nota-fiscal3.serie       = det-rds-dg.serie-nota-carga 
             and   b-nota-fiscal3.nr-nota-fis = string(det-rds-dg.nr-nota-carga,"9999999") no-error.
        if available b-nota-fiscal3 THEN DO:
            find first tt-data no-lock
                 where tt-data.dt-emis-nota = b-nota-fiscal3.dt-emis-nota  
                 and   tt-data.serie        = b-nota-fiscal3.serie no-error.
            if not available tt-data then  do:
                create tt-data.
                assign tt-data.dt-emis-nota = b-nota-fiscal3.dt-emis-nota
                       tt-data.serie        = b-nota-fiscal3.serie.       
            end.
            
            find first tt-notas where tt-notas.nr-nota-fis  = b-nota-fiscal3.nr-nota-fis no-lock no-error.
            if not available tt-notas then do:
                create tt-notas.
                assign tt-notas.dt-emis-nota = b-nota-fiscal3.dt-emis-nota
                       tt-notas.serie        = b-nota-fiscal3.serie
                       tt-notas.nr-nota-fis  = b-nota-fiscal3.nr-nota-fis.
            end.
        end.
    end.

    for each tt-data
          by tt-data.serie
          by tt-data.dt-emis-nota:
    
        assign c-serie = tt-data.serie
               dt-data = tt-data.dt-emis-nota.
    
        for first tt-notas
            where tt-notas.dt-emis-nota = tt-data.dt-emis-nota 
            and   tt-notas.serie        = tt-data.serie:
    
            assign c-nr-nota-ini = tt-notas.nr-nota-fis.
        end.
    
        for last tt-notas 
            where tt-notas.dt-emis-nota = tt-data.dt-emis-nota 
            and   tt-notas.serie        = tt-data.serie:
    
            assign c-nr-nota-fim = tt-notas.nr-nota-fis.
        end.
    
        assign c-compl-carga = c-compl-carga + chr(10) + chr(32) + chr(32) + chr(32) + chr(32) + chr(32) + chr(32) + chr(32) + chr(32) + chr(32) + chr(32).
               c-compl-carga = c-compl-carga + c-nr-nota-ini + c-nr-nota-fim + c-serie + string(dt-data,"99/99/9999").
    end.

    
    find first det-rds-dg no-lock
         where det-rds-dg.estab-compl-carga  = nota-fiscal.cod-estabel      
         and   det-rds-dg.serie-compl-carga  = nota-fiscal.serie              
         and   det-rds-dg.nr-compl-carga     = integer(nota-fiscal.nr-nota-fis) no-error.
    if available det-rds-dg then        
        assign l-nf-compl-carg = yes.
    else
        assign l-nf-compl-carg = no.

    if  lComplementarDeEntrada or l-nf-compl-carg  then do:

        IF estabelec.estado = "MG" THEN NEXT.
        
        if not c-mensagem2 matches "*Nota Fiscal complementar de ICMS referente as nossas NF*" then
            assign c-mensagem2 = "Nota Fiscal complementar de ICMS referente as nossas NF N" + chr(186) + " " + 
                                 string(c-nr-nota-ini) + " atÇ "  + string(c-nr-nota-fim) + " emitida em " + 
                                 if dt-data <> ? then string(dt-data,"99/99/9999") else "".
        
        if c-nr-nota-ini = "" or c-nr-nota-ini = c-nr-nota-fim then do:

            find first det-rds-dg no-lock
                 where det-rds-dg.estab-compl-entrada = nota-fiscal.cod-estabel  
                 and   det-rds-dg.serie-compl-entrada = nota-fiscal.serie  
                 and   det-rds-dg.nr-compl-entrada    = integer(nota-fiscal.nr-nota-fis)  
                 and   det-rds-dg.situacao            = 1 no-error.
            if not AVAILABLE det-rds-dg then
                find first det-rds-dg no-lock
                     where det-rds-dg.estab-compl-carga = nota-fiscal.cod-estabel  
                     and   det-rds-dg.serie-compl-carga = nota-fiscal.serie  
                     and   det-rds-dg.nr-compl-carga    = integer(nota-fiscal.nr-nota-fis)  
                     and   det-rds-dg.situacao          = 1 no-error.
    
            if not available det-rds-dg then do:
                if not c-mensagem2 matches "*Nota fiscal complementar de ICMS referente a nossa NF*" then
                    assign c-mensagem2 = "Nota fiscal complementar de ICMS referente a nossa NF n" + chr(186) + " " +  
                                         string(it-nota-fisc.nr-docum,"x(16)") + 
                                         " emitida em " +  
                                         (if dt-data <> ? then string(dt-data,"99/99/9999") else string(nota-fiscal.dt-emis-nota,"99/99/9999")) .
            end. else do:
                find first ret-cil-dg no-lock 
                     where ret-cil-dg.cod-estabel = det-rds-dg.cod-estabel 
                     and   ret-cil-dg.serie-rds   = det-rds-dg.serie-rds   
                     and   ret-cil-dg.nr-rds      = det-rds-dg.nr-rds no-error.
                if not available ret-cil-dg then do:
                    if not c-mensagem2 matches "*Nota fiscal complementar de ICMS referente a nossa NF*" then
                        assign c-mensagem2 = "Nota fiscal complementar de ICMS referente a nossa NF n" + chr(186) + " " +  
                                             string(it-nota-fisc.nr-docum,"x(16)") + 
                                             " emitida em " +  
                                             (if dt-data <> ? then STRING(dt-data,"99/99/9999") else string(nota-fiscal.dt-emis-nota,"99/99/9999")) .
                end. else do:
                    find first b-nota-fiscal-compl no-lock
                         where b-nota-fiscal-compl.cod-estabel = ret-cil-dg.est-nota-entrada   
                         and   b-nota-fiscal-compl.serie       = ret-cil-dg.serie-nota-entrada 
                         and   b-nota-fiscal-compl.nr-nota-fis = string(ret-cil-dg.nr-nota-entrada, "9999999") no-error.
                    if available b-nota-fiscal-compl then do:
                        if not c-mensagem2 matches "*Nota fiscal complementar de ICMS referente a nossa NF*" then
                            assign c-mensagem2 = "Nota fiscal complementar de ICMS referente a nossa NF n" + chr(186) + " " +  
                                                 string(b-nota-fiscal-compl.nr-nota-fis,"x(16)") + 
                                                 " emitida em " +  
                                                 string(b-nota-fiscal-compl.dt-emis-nota,"99/99/9999").
                    end. else do:
                        if dt-data = ? then do:
                            find first b-nota-fiscal2 no-lock 
                                 where b-nota-fiscal2.cod-estabel = it-nota-fisc.cod-estabel 
                                 and   b-nota-fiscal2.serie       = it-nota-fisc.serie-docum 
                                 and   b-nota-fiscal2.nr-nota-fis = it-nota-fisc.nr-docum  no-error.
                            if available b-nota-fiscal2 then
                                assign dt-data = b-nota-fiscal2.dt-emis-nota.
                        end.
    
                        if it-nota-fisc.nr-docum <> "" then do:
                            if not c-mensagem2 matches "*Nota fiscal complementar de ICMS referente a nossa NF*" then
                                assign c-mensagem2 = "Nota fiscal complementar de ICMS referente a nossa NF n" + chr(186) + " " +  
                                                     string(it-nota-fisc.nr-docum,"x(16)") + " emitida em " + 
                                                     (if dt-data <> ? then string(dt-data,"99/99/9999") else string(nota-fiscal.dt-emis-nota,"99/99/9999")).
                        end. else do:
                            find first it-ordem-carreg no-lock
                                 where it-ordem-carreg.cod-estabel = det-rds-dg.cod-estabel 
                                 and   it-ordem-carreg.serie-rds   = det-rds-dg.serie-rds     
                                 and   it-ordem-carreg.nr-rds      = det-rds-dg.nr-rds no-error.
                            if available it-ordem-carreg then do:
                                find first b-nota-fiscal-compl no-lock
                                     where b-nota-fiscal-compl.cod-estabel = it-ordem-carreg.estab-nota-carga 
                                     and   b-nota-fiscal-compl.serie       = it-ordem-carreg.serie-nota-carga 
                                     and   b-nota-fiscal-compl.nr-nota-fis = string(it-ordem-carreg.nr-nota-carga, "9999999") no-error.
                                if available b-nota-fiscal-compl then do:
                                    if not c-mensagem2 matches "*Nota fiscal complementar de ICMS referente a nossa NF*" then
                                        assign c-mensagem2 = "Nota fiscal complementar de ICMS referente a nossa NF n" + chr(186) + " " +  
                                                             string(b-nota-fiscal-compl.nr-nota-fis,"x(16)") + 
                                                             " emitida em " +  
                                                             string(b-nota-fiscal-compl.dt-emis-nota,"99/99/9999").         
                                end.
                            end.    
                        end.
                    end.
                end.
            end.
        end.    
    end.

    assign l-desc-icms = no.
    
    find first ext-emitente no-lock 
         where ext-emitente.cod-emitente = emitente.cod-emitente no-error.

    if available ext-emitente then 
        find first desc-icms no-lock
             where desc-icms.cod-estabel = nota-fiscal.cod-estabel  
             and   desc-icms.cod-tipo    = ext-emitente.cod-tipo no-error. 

    if available desc-icms then do:
        
        find first bf-icms-estabelec no-lock
             where bf-icms-estabelec.cod-estabel = nota-fiscal.cod-estabel no-error.

        find first unid-feder no-lock
             where unid-feder.estado = bf-icms-estabelec.estado no-error.
        
        if available bf-icms-estabelec and 
           available unid-feder        and 
           emitente.estado = bf-icms-estabelec.estado and 
           natur-oper.emite-duplic then 
            assign l-desc-icms   = yes
                   de-valor-icms = nota-fiscal.vl-tot-nota / (100 - unid-feder.per-icms-int) * 100.        
    end.

    if l-desc-icms then do:    
        
        if not c-mensagem2 matches "*LOR DO ICMS (DESC. EM RAZAO DA ISENCAO*" then
            assign c-mensagem2 = c-mensagem2 + " " + 
                  "VALOR TOTAL COM ICMS                      R$" + string(de-valor-icms,">>,>>>,>>>,>>9.99") + " " + 
                  "VALOR DO ICMS (DESC. EM RAZAO DA ISENCAO) R$" + string((de-valor-icms - nota-fiscal.vl-tot-nota),">>,>>>,>>>,>>9.99") + " " +
                  "VALOR TOTAL SEM ICMS                      R$" + string(nota-fiscal.vl-tot-nota,">>,>>>,>>>,>>9.99") + " ".

        find first mensagem no-lock 
             where mensagem.cod-mensagem = desc-icms.cod-mensagem no-error.
        if available mensagem then do:
            if not c-mensagem2 matches "*" + replace(replace(mensagem.texto,chr(13),""),chr(10),"") + "*" then
                assign c-mensagem2 = c-mensagem2 + " " + replace(replace(mensagem.texto,chr(13),""),chr(10),"").
        end.

        assign de-val9100 = (de-valor-icms - nota-fiscal.vl-tot-nota)
               i-num9100  = 2
               i-tam9100  = 130.
        
        run "cdp/cd9100.p".
        
        assign c-ext9100[1] = replace(c-ext9100[1],"*","") 
               c-ext9100[2] = replace(c-ext9100[2],"*","").
        
        if estabelec.estado = "RJ" then do:
            
            if not c-mensagem2 matches "*Valor dispensado de R$*" then
                assign c-mensagem2 = c-mensagem2 + " " + "Valor dispensado de R$ " + string((de-valor-icms - nota-fiscal.vl-tot-nota),">>>,>>>,>>9.99").
            
            IF c-ext9100[2] > "" then do:
                if not c-mensagem2 matches "*" + " (" + c-ext9100[1] + c-ext9100[2] + ")" + "*" then
                    assign c-mensagem2 = c-mensagem2 + " " + " (" + c-ext9100[1] + c-ext9100[2] + ")".
            end.
            else do:
                if not c-mensagem2 matches "*" + " (" + c-ext9100[1] + ")" + "*" then
                    assign c-mensagem2 = c-mensagem2 + " " + " (" + c-ext9100[1] + ")".
            end.
        end.     
    end.

    for each b-it-nota-fisc of nota-fiscal no-lock:

        if natur-oper.tipo <> 1 then do:
            
            find first bf-alb-it-nota-fisc of b-it-nota-fisc no-lock no-error.
            
            if natur-oper.tp-rec-desp = 100 then do:
                
                find first dif-icms no-lock
                     where dif-icms.cod-emitente = nota-fiscal.cod-emitente  
                     and   dif-icms.flag-aplicar 
                     and   dif-icms.dif-icms-ativo no-error.
                if not available dif-icms then 
                    find first dif-icms no-lock
                        where dif-icms.cod-emitente     = nota-fiscal.cod-emitente 
                        and   dif-icms.cod-produto-coml = bf-alb-it-nota-fisc.cod-produto-coml  
                        and   dif-icms.dif-icms-ativo no-error.
            end.
            
            if available dif-icms and bf-alb-it-nota-fisc.desc-icms > 0 AND dif-icms.acresc-icms = NO then do:          
                
                assign acum-valor-icms = 0.
                
                for each bf-it-nota-fisc2 no-lock of nota-fiscal:
                    find first bf-alb-it-nota-fisc of bf-it-nota-fisc2 no-lock no-error.
                    
                    assign acum-valor-icms = acum-valor-icms + (bf-alb-it-nota-fisc.desc-icms + bf-alb-it-nota-fisc.desc-icms-frete + bf-alb-it-nota-fisc.desc-icms-freteg).
                    
                end.
                
                find first mensagem no-lock 
                     where mensagem.cod-mensag = dif-icms.cod-mensagem no-error.
                if available mensagem then do:
                    if not c-mensagem2 matches "*" + mensagem.texto-mensag + "*" then
                        ASSIGN c-mensagem2 = c-mensagem2 + "  " + mensagem.texto-mensag.
                end.
            end.
        end.
    end.

    if acum-valor-icms > 0 then do:
        
        if not c-mensagem2 matches "*Valor do ICMS Diferido R$*" then
            assign c-mensagem2 = c-mensagem2 + " Valor do ICMS Diferido R$ " + string(acum-valor-icms,">>>,>>9.99")
                   de-val9100 = (acum-valor-icms)
                   i-num9100  = 2
                   i-tam9100  = 130.
        
        run "cdp/cd9100.p".
        
        assign c-ext9100[1] = replace(c-ext9100[1],"*","") 
               c-ext9100[2] = replace(c-ext9100[2],"*","").
        
        if c-ext9100[2] > "" then do:
            if not c-mensagem2 matches "*" + " ( " + c-ext9100[1] + c-ext9100[2] + ")" + "*" then
                assign c-mensagem2 = c-mensagem2 + " ( " + c-ext9100[1] + c-ext9100[2] + ")".
        end.
        else do:
            if not c-mensagem2 matches "*" + " ( " + c-ext9100[1] + ")" + "*" then
                assign c-mensagem2 = c-mensagem2 + " ( " + c-ext9100[1] + ")".
        end.
        
    end.
    
    if available cond-pagto and natur-oper.emite-duplic then do:
       
        if not c-mensagem2 matches "*Condicao de Pagamento: " + "*" THEN  
            assign c-mensagem2 = c-mensagem2 + " " + " Condicao de Pagamento: " + string(cond-pagto.cod-cond-pag) + " - " + cond-pagto.descricao + "  ".
    end.
        
    if available natur-oper and natur-oper.tipo = 2 and natur-oper.per-des-icms <> 0 then do:
    
        assign de-total-bas-icm = 0.
        
        for each it-nota-fisc no-lock of nota-fiscal,

            each item no-lock
           where item.it-codigo = it-nota-fisc.it-codigo:
            
            assign de-total-bas-icm = de-total-bas-icm + it-nota-fisc.vl-bicms-it.
        end.

        if not c-mensagem2 matches "*BASE CALC REDUZIDA*" then
            assign c-mensagem2 = c-mensagem2 + "  " + 
                                 "BASE CALC: " + string(nota-fiscal.vl-mercad / (100 - natur-oper.per-des-icms) * 100,">>>,>>>,>>9.99") +
                                 " REDUÄ«O: " + trim(string(natur-oper.per-des-icms)) +
                                 " BASE CALC REDUZIDA: " + string(de-total-bas-icm).
    end.

    if available ext-natur-oper and ext-natur-oper.id-nf-cobert-carga = yes then do:

        find first det-rds no-lock
             where det-rds.estab-nota-carga = nota-fiscal.cod-estabel 
             and   det-rds.serie-nota-carga = nota-fiscal.serie       
             and   det-rds.nr-nota-carga    = integer(nota-fiscal.nr-nota-fis) no-error.
        if available det-rds then do:
            
            find first rds of det-rds no-lock no-error.
            if available rds and not c-mensagem2 matches "*RDS*" then do:
                if not c-mensagem2 matches "*" + " RDS: " + string(rds.nr-rds) + "/" + string(day(nota-fiscal.dt-emis-nota)) + "*" then
                    assign c-mensagem2 = c-mensagem2 + " RDS: " + string(rds.nr-rds) + "/" + string(day(nota-fiscal.dt-emis-nota)).
            end.
            
            find first ext-estabelec no-lock 
                 where ext-estabelec.cod-estabel = rds.cod-estabel no-error.
            if available ext-estabelec and ext-estabelec.imprime-coletor then do:
                find first coletor no-lock 
                     where coletor.cod-coletor = rds.cod-coletor  
                     and   coletor.cod-estabel = rds.cod-estabel no-error.
            end.
            
            assign w-form-de       = (if AVAILABLE ext-estabelec and ext-estabelec.imprime-forms   then rds.form-de     else 0)
                   w-form-ate      = (if AVAILABLE ext-estabelec and ext-estabelec.imprime-forms   then rds.form-ate    else 0)
                   w-form2-de      = (if AVAILABLE ext-estabelec and ext-estabelec.imprime-forms   then rds.form2-de    else 0)
                   w-form2-ate     = (if AVAILABLE ext-estabelec and ext-estabelec.imprime-forms   then rds.form2-ate   else 0)
                   w-cod-coletor   = (if AVAILABLE ext-estabelec and ext-estabelec.imprime-coletor then rds.cod-coletor else "")
                   w-serie-coletor = (if AVAILABLE ext-estabelec and ext-estabelec.imprime-coletor then coletor.serie   else "").
            
            if estabelec.cod-estabel = '10' then do:
                
                if not c-mensagem2 matches "*RIOS DE:*" then do:
                    if (w-form-de  <> 0 and w-form-ate <> 0 and ext-estabelec.imprime-forms) and ext-estabelec.imprime-coletor then
                        assign c-mensagem2 = c-mensagem2 + " FORMULµRIOS DE: " + trim(string(w-form-de,"999999999")) + " ATê: " + trim(string(w-form-ate,"999999999")) + 
                                                           " DE: " + trim(string(w-form2-de,"999999999")) + " ATê: " + trim(string(w-form2-ate,"999999999")) +
                                                           " Nr. da Ordem: " + trim(string(w-serie-coletor)) + " Coletor: " + trim(string(w-cod-coletor)).
                    else 
                        if (w-form-de  <> 0 and w-form-ate <> 0 and ext-estabelec.imprime-forms) and not ext-estabelec.imprime-coletor then
                            assign c-mensagem2 = c-mensagem2 + " FORMULµRIOS DE: " + trim(string(w-form-de,"999999999")) + " ATê: " + trim(string(w-form-ate,"999999999")) +
                                                               " DE: " + trim(string(w-form2-de,"999999999")) + " ATê: " + trim(string(w-form2-ate,"999999999")).
                        else 
                            if not (w-form-de  <> 0 and w-form-ate <> 0 and ext-estabelec.imprime-forms) and ext-estabelec.imprime-coletor then
                                assign c-mensagem2 = c-mensagem2 + " Nr. da Ordem: " + trim(string(w-serie-coletor)) + " Coletor: " + trim(string(w-cod-coletor)).
                            else 
                                if not (w-form-de  <> 0 and w-form-ate <> 0 and ext-estabelec.imprime-forms) and not ext-estabelec.imprime-coletor then
                                    assign c-mensagem2 = c-mensagem2 + "".
                end.
            end. 
            else do:
                IF NOT c-mensagem2 MATCHES "*RIOS DE:*" THEN DO:
        
                    find first alb-nota-fiscal of nota-fiscal no-lock no-error.
                    if (w-form-de  <> 0 and w-form-ate <> 0 and ext-estabelec.imprime-forms) and ext-estabelec.imprime-coletor then
                        assign  c-mensagem2 = c-mensagem2 + " FORMULµRIOS DE: " + trim(string(w-form-de,"999999999")) + " ATê: " + trim(string(w-form-ate,"999999999")) + 
                                                           " DE: " + trim(string(w-form2-de,"999999999")) + " ATê: " + trim(string(w-form2-ate,"999999999")) + 
                                                           " SÇrie: " + trim(string(w-serie-coletor)) + " Coletor: " + trim(string(w-cod-coletor)).
                    else 
                        if (w-form-de  <> 0 and w-form-ate <> 0 and ext-estabelec.imprime-forms) and not ext-estabelec.imprime-coletor then
                            assign c-mensagem2 = c-mensagem2 + " FORMULµRIOS DE: " + trim(string(w-form-de,"999999999")) + " ATê: " + trim(string(w-form-ate,"999999999")) +
                                                               " DE: " + trim(string(w-form2-de,"999999999")) + " ATê: " + trim(string(w-form2-ate,"999999999")).
                        else 
                            if not (w-form-de  <> 0 and w-form-ate <> 0 and ext-estabelec.imprime-forms) and ext-estabelec.imprime-coletor then
                                assign c-mensagem2 = c-mensagem2 + " SÇrie: " + trim(string(w-serie-coletor)) + " Coletor: " + trim(string(w-cod-coletor)).
                            else do:
                                if not (w-form-de  <> 0 and w-form-ate <> 0 and ext-estabelec.imprime-forms) and not ext-estabelec.imprime-coletor then
                                    assign c-mensagem2 = c-mensagem2 + "".
                            end.
                end.
            end.
        end.

        find first det-rds no-lock
             where det-rds.est-nota-saida   = nota-fiscal.cod-estabel
             and   det-rds.serie-nota-saida = nota-fiscal.serie    
             and   det-rds.nr-nota-saida    = integer(nota-fiscal.nr-nota-fis) no-error.
        if available det-rds then do: 
            find first rds of det-rds no-lock no-error.
            if available rds and not c-mensagem2 matches "*RDS*" then
                assign c-mensagem2 = c-mensagem2 + " RDS: " + string(rds.nr-rds) + "/" + string(day(nota-fiscal.dt-emis-nota)).
            
            find first rota-liq of rds no-lock no-error.
            if rota-liq.emissao-doc = 1 then do: /* EMS */
                
                find first prod-rota of det-rds no-lock no-error.
                if available prod-rota then do:
                    
                    if not c-mensagem2 matches "*Tipo de Entrega*" then do:
        
                        assign c-mensagem2 = c-mensagem2 + " Tipo de entrega = ".
                        
                        if prod-rota.tipo-cobranca = 1 then
                            assign c-mensagem2 = c-mensagem2 + " 1 - entrega normal".
                        else
                        if prod-rota.tipo-cobranca = 2 or 
                           prod-rota.tipo-cobranca = 3 then
                            assign c-mensagem2 = c-mensagem2 + " 2 - entrega emergencial".
                        else     
                        if prod-rota.tipo-cobranca = 4 then
                            assign c-mensagem2 = c-mensagem2 + " 4 - sem cobranca".
                    end.                    
                end. 

                if not c-mensagem2 matches "*Frete a Granel*" then
                    assign c-mensagem2 = c-mensagem2 + " FG = Frete a Granel".
            end.
        end.
        
        find first det-rds-dg no-lock
             where det-rds-dg.estab-nota-carga = nota-fiscal.cod-estabel 
             and   det-rds-dg.serie-nota-carga = nota-fiscal.serie       
             and   det-rds-dg.nr-nota-carga    = integer(nota-fiscal.nr-nota-fis) no-error.
        if available det-rds-dg then do: 
            
            find first rds-dg OF det-rds-dg no-lock no-error.
            if available rds-dg and NOT c-mensagem2 matches "*RDS*" then 
                assign c-mensagem2 = c-mensagem2 + " RDS: " + string(rds-dg.nr-rds) + "/" + string(day(nota-fiscal.dt-emis-nota)).
            
            find first equipe-coletor no-lock
                 where equipe-coletor.cod-equipe  = rds-dg.cod-equipe  
                   and equipe-coletor.cod-estabel = rds-dg.cod-estabel no-error.
            if available equipe-coletor then do:
                
                find first coletor no-lock
                     where coletor.cod-coletor = equipe-coletor.cod-coletor 
                     and   coletor.cod-estabel = equipe-coletor.cod-estabel /* 00632 */ no-error.
                if available coletor                    and 
                   equipe-coletor.form-de  <> 0         and 
                   equipe-coletor.form-ate <> 0         and 
                   not c-mensagem2 matches "*RIOS DE:*" then
                    assign c-mensagem2 = c-mensagem2 + " " + " FORMULµRIOS DE:" + trim(string(equipe-coletor.form-de)) + " ATê: " + trim(string(equipe-coletor.form-ate)) + 
                                         "  SÇrie: " + trim(string(coletor.serie)) + 
                                         " Coletor " + trim(string(equipe-coletor.cod-coletor)) + 
                                         " DE:" + trim(string(equipe-coletor.form2-de)) + " ATê: " + trim(string(equipe-coletor.form2-ate)).
            end.
        end.
    end.

    find first alb-it-nota-fisc of nota-fiscal no-lock no-error.
    if available alb-it-nota-fisc then do:
        find FIRST pol-coml OF alb-it-nota-fisc no-lock no-error.
        if available pol-coml and pol-coml.nr-pedcli <> "" then do:
            if not c-mensagem2 matches "*" + " N£mero do pedido do cliente: " + pol-coml.nr-pedcli + "*" then
                assign c-mensagem2 = c-mensagem2 + " N£mero do pedido do cliente: " + pol-coml.nr-pedcli.
        end.
    end.

    find first ext-ser-estab no-lock 
         where ext-ser-estab.serie         = nota-fiscal.serie         
         and   ext-ser-estab.cod-estabel   = nota-fiscal.cod-estabel no-error.
    if available ext-ser-estab 
    and ext-ser-estab.tipo-docto = 19 /* NF de Serviáo */       
    or ext-ser-estab.tipo-docto = 20 /* NF de Ent e Saida */   
    or ext-ser-estab.tipo-docto = 21 /* NF de Locacao*/ then
        if not c-mensagem2 matches "*08007730322*" then do:
            if not c-mensagem2 matches "*" + "    SAC Vitalaire: 08007730322" + "*" then
                assign c-mensagem2 = c-mensagem2 + "    SAC Vitalaire: 08007730322". 
        end.

    IF ttDanfe.informacoescomplementares MATCHES "*RICMS/PR.*" THEN
       ASSIGN c-mensagem2 = c-mensagem2 + SUBSTRING(ttDanfe.informacoescomplementares,INDEX(ttDanfe.informacoescomplementares,"| opera"),INDEX(ttDanfe.informacoescomplementares,"/PR.") - INDEX(ttDanfe.informacoescomplementares,"| opera") + 4) NO-ERROR.

    run pi-print-editor(input c-mensagem2, input 68).
    
    assign c-mensagem2 = "".
    for each tt-editor:
        if not c-mensagem2 matches "*" + tt-editor.conteudo + "*" then
            assign c-mensagem2 = c-mensagem2 + " " + tt-editor.conteudo.
    end. 

    assign ttDanfe.informacoescomplementares = c-mensagem2.

    ASSIGN ttDanfe.informacoescomplementares = TRIM(ttDanfe.informacoescomplementares)
           ttDanfe.informacoescomplementares = REPLACE(ttDanfe.informacoescomplementares, "~n", ' ')
           ttDanfe.informacoescomplementares = REPLACE(ttDanfe.informacoescomplementares, "~t", ' ').

    DO WHILE ttDanfe.informacoescomplementares <> REPLACE(ttDanfe.informacoescomplementares, "  ", ' '):
        ASSIGN ttDanfe.informacoescomplementares = REPLACE(ttDanfe.informacoescomplementares, "  ", ' ').
    END.

END PROCEDURE.

PROCEDURE pi-transportadora:
    IF AVAILABLE nota-fiscal THEN DO:
        IF NOT VALID-HANDLE(h-boal042ef) THEN RUN albo/boal042ef.p PERSISTENT SET h-boal042ef.
            
        RUN pi-dados-transp-nfe IN h-boal042ef ( INPUT ROWID(nota-fiscal),
                                                 INPUT-OUTPUT c-pago,
                                                 INPUT-OUTPUT ttDanfe.codantt1, 
                                                 INPUT-OUTPUT ttDanfe.placa1,
                                                 INPUT-OUTPUT ttDanfe.ufpl1,
                                                 INPUT-OUTPUT ttDanfe.enderecotransp,
                                                 INPUT-OUTPUT ttDanfe.cidadetransp,
                                                 INPUT-OUTPUT ttDanfe.uftran,
                                                 INPUT-OUTPUT ttDanfe.inscrestadtransp, 
                                                 INPUT-OUTPUT ttDanfe.cnpjtransp ).

        IF nota-fiscal.placa = "" OR nota-fiscal.uf-placa = "" THEN ASSIGN ttDanfe.codantt1 = "".

        IF VALID-HANDLE(h-boal042ef) THEN DELETE PROCEDURE h-boal042ef.

        CASE c-pago:
            WHEN "0" THEN ASSIGN ttDanfe.idfr = "0-Emitente".
            WHEN "1" THEN ASSIGN ttDanfe.idfr = "1-Destinat†rio".
            WHEN "9" THEN ASSIGN ttDanfe.idfr = "9-Outros".
        END CASE.
    END.
END PROCEDURE.

procedure pi-valores-item:
    if can-find(first cidade-zf no-lock
                where cidade-zf.cidade = nota-fiscal.cidade
                and   cidade-zf.estado = nota-fiscal.estado) then do:

            if natur-oper.log-deduz-desc-zfm-tot-nf then
                assign de-vl-unit  = ( if it-nota-fisc.ind-fat-qtfam = no 
                                       then ( it-nota-fisc.vl-preuni / de-conv-total )
                                       else ( ( ( it-nota-fisc.vl-preuni * it-nota-fisc.qt-faturada[1] ) / de-conv-total ) / it-nota-fisc.qt-faturada[2] ) )
                       de-vl-total = round(( ( it-nota-fisc.vl-preuni *  it-nota-fisc.qt-faturada[1] )/ de-conv-total),2).
            ELSE
                assign de-vl-unit  = ( if it-nota-fisc.ind-fat-qtfam = no 
                                          then ( it-nota-fisc.vl-preuni / de-conv )
                                          else ( ( ( it-nota-fisc.vl-preuni * it-nota-fisc.qt-faturada[1] ) / de-conv ) / it-nota-fisc.qt-faturada[2] ) )
                       de-vl-total = round(( ( it-nota-fisc.vl-preuni *  it-nota-fisc.qt-faturada[1] ) / de-conv),2 ).

        if de-vl-unit = ? then assign de-vl-unit = 0.

        if de-vl-total = ? then assign de-vl-total = 0.

        if natur-oper.log-deduz-desc-zfm-tot-nf then
            assign de-vl-unit-trib = ( it-nota-fisc.vl-preuni / de-conv-total ).
        else
            assign de-vl-unit-trib = ( it-nota-fisc.vl-preuni / de-conv ).

        if it-nota-fisc.aliquota-ipi <> 0 or it-nota-fisc.vl-ipi-it <> 0 or nota-fiscal.esp-docto <> 20 then
            if it-nota-fisc.vl-despes-it > 0 and 
               it-nota-fisc.vl-ipi-it    > 0 and 
               it-nota-fisc.cd-trib-ipi <> 3 then do:

                {ftp/ft0513.i1}  /* Determina valor do IPI */

                if natur-oper.log-deduz-desc-zfm-tot-nf then 
                    assign de-vl-ipi-it = de-vl-ipi-it / de-conv-total.
                else
                    assign de-vl-ipi-it = de-vl-ipi-it / de-conv.
            end.
            else
                if it-nota-fisc.cd-trib-ipi <> 3 then do:
                    if natur-oper.log-deduz-desc-zfm-tot-nf THEN
                        assign de-vl-ipi-it = it-nota-fisc.vl-ipi-it / de-conv-total.
                    else
                        assign de-vl-ipi-it = it-nota-fisc.vl-ipi-it / de-conv.
                end.

        if c-modelo-DANFE = '3' then 
            assign ttDanfeItem.vlunit       = string(de-vl-unit, ">>>,>>>,>>9.9999")
                   ttDanfeItem.vltotitem    = string(de-vl-total, ">>>,>>>,>>9.99")
                   ttDanfeItem.vlbcicmit    = string(if (it-nota-fisc.cd-trib-icm = 3 and l-icms-outras-it) then it-nota-fisc.vl-icmsou-it else it-nota-fisc.vl-bicms-it, ">>,>>>,>>9.99")
                   ttDanfeItem.vlicmit      = string(if it-nota-fisc.cd-trib-icm <> 3 or (it-nota-fisc.cd-trib-icm = 3 and l-icms-outras-it) then it-nota-fisc.vl-icms-it else 0,">>>>,>>9.99")
                   ttDanfeItem.icm          = string(if it-nota-fisc.cd-trib-icm <> 3 or (it-nota-fisc.cd-trib-icm = 3 and l-icms-outras-it) then it-nota-fisc.aliquota-icm else 0,">>9.99")
                   ttDanfeItem.vlipiit      = string(de-vl-ipi-it, ">>>>,>>9.99")
                   ttDanfeItem.ipi          = string(it-nota-fisc.aliquota-ipi, ">>9.99").
        else                                
            assign ttDanfeItem.vlunit       = string(de-vl-unit, ">>>,>>>,>>9.9999")
                   ttDanfeItem.vltotitem    = string(de-vl-total, ">>>,>>>,>>9.99")
                   ttDanfeItem.vlbcicmit    = string(if (it-nota-fisc.cd-trib-icm  = 3 and l-icms-outras-it) then it-nota-fisc.vl-icmsou-it else it-nota-fisc.vl-bicms-it, ">>,>>>,>>9.99")
                   ttDanfeItem.vlicmit      = string(if it-nota-fisc.cd-trib-icm <> 3 or (it-nota-fisc.cd-trib-icm = 3 and l-icms-outras-it) then it-nota-fisc.vl-icms-it else 0,">>,>>>,>>9.99")
                   ttDanfeItem.icm          = string(if it-nota-fisc.cd-trib-icm <> 3 or (it-nota-fisc.cd-trib-icm = 3 and l-icms-outras-it) then it-nota-fisc.aliquota-icm else 0,">>9.99")
                   ttDanfeItem.vlipiit      = string(de-vl-ipi-it, ">>>,>>9.99")
                   ttDanfeItem.ipi          = string(it-nota-fisc.aliquota-ipi, ">>9.99")
                   ttDanfeItem.vlbcicmit-st = string(it-nota-fisc.vl-bsubs-it, ">>,>>>,>>9.99")
                   ttDanfeItem.vlicmit-st   = string(it-nota-fisc.vl-icmsub-it, ">>,>>>,>>9.99").

        assign l-imprimiu-val = yes.
    end. 
    else do:

       assign de-vl-unit  = /*round( */ ( if it-nota-fisc.ind-fat-qtfam = no 
                                     then it-nota-fisc.vl-preuni
                                     else ( ( it-nota-fisc.vl-preuni * it-nota-fisc.qt-faturada[1] ) / it-nota-fisc.qt-faturada[2] ) ) /*,2)*/
              de-vl-total = it-nota-fisc.vl-merc-liq.

       find first bf_alb-it-nota-fisc_aux of it-nota-fisc no-lock no-error.
       if AVAILABLE bf_alb-it-nota-fisc_aux and
          bf_alb-it-nota-fisc_aux.nr-politica <> 0 then do:

           find first pol-coml where pol-coml.nr-politica = bf_alb-it-nota-fisc_aux.nr-politica no-lock no-error.
           if AVAILABLE pol-coml then do:

               assign de-vl-unit = if not it-nota-fisc.ind-fat-qtfam then it-nota-fisc.vl-preuni
                                                                     else ( ( it-nota-fisc.vl-preuni * it-nota-fisc.qt-faturada[1] ) / it-nota-fisc.qt-faturada[2] ).

               if round( dec(ttDet.vUnCom), pol-coml.nr-casas-decimais ) <> de-vl-unit then
                   assign 
                       ttDanfeItem.vlunit = string( round( de-vl-unit, pol-coml.nr-casas-decimais ), ">>>,>>>,>>9.9999").

           end.

       end.

       if de-vl-unit = ? then assign de-vl-unit = 0.

       if de-vl-total = ? then assign de-vl-total = 0.

        if c-modelo-DANFE = '3' then
            assign ttDanfeItem.vltotitem = string(de-vl-total,                  ">>>,>>>,>>9.99").
        else
            assign ttDanfeItem.vltotitem = string(de-vl-total,                  ">>>,>>>,>>9.99").

        if c-modelo-DANFE = '3' then
            assign ttDanfeItem.vlbcicmit    = string(if (it-nota-fisc.cd-trib-icm = 3 and l-icms-outras-it) then it-nota-fisc.vl-icmsou-it else it-nota-fisc.vl-bicms-it,  ">,>>>>,>>9.99")
                   ttDanfeItem.vlicmit      = string(if it-nota-fisc.cd-trib-icm <> 3 or (it-nota-fisc.cd-trib-icm = 3 and l-icms-outras-it) then it-nota-fisc.vl-icms-it   else 0, ">>>>,>>9.99")
                   ttDanfeItem.icm          = string(if it-nota-fisc.cd-trib-icm <> 3 or (it-nota-fisc.cd-trib-icm = 3 and l-icms-outras-it) then it-nota-fisc.aliquota-icm else 0,">>9.99").
        else
            assign ttDanfeItem.vlbcicmit    = string(if (it-nota-fisc.cd-trib-icm = 3 and l-icms-outras-it) then it-nota-fisc.vl-icmsou-it else it-nota-fisc.vl-bicms-it,  "->,>>>,>>9.99")
                   ttDanfeItem.vlicmit      = string(if it-nota-fisc.cd-trib-icm <> 3 or (it-nota-fisc.cd-trib-icm = 3 and l-icms-outras-it) then it-nota-fisc.vl-icms-it   else 0, ">>,>>>,>>9.99")
                   ttDanfeItem.icm          = string(if it-nota-fisc.cd-trib-icm <> 3 or (it-nota-fisc.cd-trib-icm = 3 and l-icms-outras-it) then it-nota-fisc.aliquota-icm else 0,">>9.99")
                   ttDanfeItem.vlbcicmit-st = string(it-nota-fisc.vl-bsubs-it,  "->,>>>,>>9.99") /*ICMS ST - somente para layout paisagem*/
                   ttDanfeItem.vlicmit-st   = string(it-nota-fisc.vl-icmsub-it, ">>,>>>,>>9.99").

        assign l-imprimiu-val-icm = yes.

        if it-nota-fisc.aliquota-ipi <> 0 or  
           it-nota-fisc.vl-ipi-it    <> 0 or  
           nota-fiscal.esp-docto     <> 20 then
            if it-nota-fisc.vl-despes-it > 0 and
               it-nota-fisc.vl-ipi-it    > 0 and 
               it-nota-fisc.cd-trib-ipi <> 3 then do:

                {ftp/ft0513.i1}  /* Determina valor do IPI */

                if c-modelo-DANFE = '3' then
                    assign ttDanfeItem.vlipiit   = string(de-vl-ipi-it,                      ">>>>,>>9.99")
                           ttDanfeItem.ipi       = string(it-nota-fisc.aliquota-ipi,              ">>9.99").
                else
                    assign ttDanfeItem.vlipiit   = string(de-vl-ipi-it,                     ">>>,>>9.99")
                           ttDanfeItem.ipi       = string(it-nota-fisc.aliquota-ipi,          ">>9.99").

                assign l-imprimiu-val = yes.
            end.
            else
                if it-nota-fisc.cd-trib-ipi <> 3 then do:
                    assign de-vl-ipi-it = it-nota-fisc.vl-ipi-it.

                    if c-modelo-DANFE = '3' THEN
                        assign ttDanfeItem.vlipiit   = string(de-vl-ipi-it,                      ">>>>,>>9.99") 
                               ttDanfeItem.ipi       = string(it-nota-fisc.aliquota-ipi,              ">>9.99").
                    else                                  
                        assign ttDanfeItem.vlipiit   = string(de-vl-ipi-it,                     ">>>,>>9.99")  
                               ttDanfeItem.ipi       = string(it-nota-fisc.aliquota-ipi,          ">>9.99"). 

                    assign l-imprimiu-val = yes.
                end. 

                if not l-imprimiu-val then do:
                    if c-modelo-DANFE = '3' then
                        assign ttDanfeItem.vlipiit = string(0.00,         ">>>>,>>9.99")         
                               ttDanfeItem.ipi     = string(0.00,              ">>9.99").              
                    else
                        assign ttDanfeItem.vlipiit = string(0.00,        ">>>,>>9.99")
                               ttDanfeItem.ipi     = string(0.00,          ">>9.99").
                end. 

                if not l-imprimiu-val-icm then do:
                    if c-modelo-DANFE = '3' then
                        assign ttDanfeItem.vlbcicmit = string(0.00,       ">,>>>>,>>9.99")       
                               ttDanfeItem.vlicmit   = string(0.00,         ">>>>,>>9.99").
                    else
                        assign ttDanfeItem.vlbcicmit = string(0.00,    "->,>>>,>>9.99")
                               ttDanfeItem.vlicmit   = string(0.00,    ">>,>>>,>>9.99").   
                end. 
    end.
end procedure.
