#lang racket
(require "query.rkt")

(define gene-list (list->set '("ENSEMBL:ENSG00000167972"
                               "ENSEMBL:ENSG00000198691"
                               "ENSEMBL:ENSG00000175164"
                               "ENSEMBL:ENSG00000278540"
                               "ENSEMBL:ENSG00000100412"
                               "ENSEMBL:ENSG00000162104"
                               "ENSEMBL:ENSG00000204305"
                               "ENSEMBL:ENSG00000135744"
                               "ENSEMBL:ENSG00000144891"
                               "ENSEMBL:ENSG00000180772"
                               "ENSEMBL:ENSG00000142208"
                               "ENSEMBL:ENSG00000148218"
                               "ENSEMBL:ENSG00000163631"
                               "ENSEMBL:ENSG00000085662"
                               "ENSEMBL:ENSG00000154188"
                               "ENSEMBL:ENSG00000091879"
                               "ENSEMBL:ENSG00000182718"
                               "ENSEMBL:ENSG00000134982"
                               "ENSEMBL:ENSG00000132703"
                               "ENSEMBL:ENSG00000023445"
                               "ENSEMBL:ENSG00000142515"
                               "ENSEMBL:ENSG00000026103"
                               "ENSEMBL:ENSG00000117560"
                               "ENSEMBL:ENSG00000240583"
                               "ENSEMBL:ENSG00000161798"
                               "ENSEMBL:ENSG00000006756"
                               "ENSEMBL:ENSG00000117601"
                               "ENSEMBL:ENSG00000149311"
                               "ENSEMBL:ENSG00000164904"
                               "ENSEMBL:ENSG00000171791"
                               "ENSEMBL:ENSG00000101144"
                               "ENSEMBL:ENSG00000010671"
                               "ENSEMBL:ENSG00000100300"
                               "ENSEMBL:ENSG00000149131"
                               "ENSEMBL:ENSG00000171860"
                               "ENSEMBL:ENSG00000106804"
                               "ENSEMBL:ENSG00000197405"
                               "ENSEMBL:ENSG00000149823"
                               "ENSEMBL:ENSG00000063180"
                               "ENSEMBL:ENSG00000164305"
                               "ENSEMBL:ENSG00000121691"
                               "ENSEMBL:ENSG00000105974"
                               "ENSEMBL:ENSG00000124813"
                               "ENSEMBL:ENSG00000149257"
                               "ENSEMBL:ENSG00000134057"
                               "ENSEMBL:ENSG00000170458"
                               "ENSEMBL:ENSG00000121594"
                               "ENSEMBL:ENSG00000101017"
                               "ENSEMBL:ENSG00000102245"
                               "ENSEMBL:ENSG00000026508"
                               "ENSEMBL:ENSG00000085063"
                               "ENSEMBL:ENSG00000129226"
                               "ENSEMBL:ENSG00000179776"
                               "ENSEMBL:ENSG00000100526"
                               "ENSEMBL:ENSG00000170835"
                               "ENSEMBL:ENSG00000001626"
                               "ENSEMBL:ENSG00000133019"
                               "ENSEMBL:ENSG00000196811"
                               "ENSEMBL:ENSG00000099622"
                               "ENSEMBL:ENSG00000122705"
                               "ENSEMBL:ENSG00000183813"
                               "ENSEMBL:ENSG00000188153"
                               "ENSEMBL:ENSG00000115966"
                               "ENSEMBL:ENSG00000167193"
                               "ENSEMBL:ENSG00000132693"
                               "ENSEMBL:ENSG00000118231"
                               "ENSEMBL:ENSG00000112062"
                               "ENSEMBL:ENSG00000101439"
                               "ENSEMBL:ENSG00000118523"
                               "ENSEMBL:ENSG00000168036"
                               "ENSEMBL:ENSG00000100448"
                               "ENSEMBL:ENSG00000166347"
                               "ENSEMBL:ENSG00000140465"
                               "ENSEMBL:ENSG00000140505"
                               "ENSEMBL:ENSG00000138061"
                               "ENSEMBL:ENSG00000160870"
                               "ENSEMBL:ENSG00000106258"
                               "ENSEMBL:ENSG00000196730"
                               "ENSEMBL:ENSG00000011465"
                               "ENSEMBL:ENSG00000159640"
                               "ENSEMBL:ENSG00000164825"
                               "ENSEMBL:ENSG00000197766"
                               "ENSEMBL:ENSG00000181019"
                               "ENSEMBL:ENSG00000213918"
                               "ENSEMBL:ENSG00000197635"
                               "ENSEMBL:ENSG00000164330"
                               "ENSEMBL:ENSG00000213694"
                               "ENSEMBL:ENSG00000078401"
                               "ENSEMBL:ENSG00000138798"
                               "ENSEMBL:ENSG00000197561"
                               "ENSEMBL:ENSG00000021355"
                               "ENSEMBL:ENSG00000066044"
                               "ENSEMBL:ENSG00000116016"
                               "ENSEMBL:ENSG00000133216"
                               "ENSEMBL:ENSG00000120915"
                               "ENSEMBL:ENSG00000130427"
                               "ENSEMBL:ENSG00000157554"
                               "ENSEMBL:ENSG00000157557"
                               "ENSEMBL:ENSG00000180210"
                               "ENSEMBL:ENSG00000117525"
                               "ENSEMBL:ENSG00000198734"
                               "ENSEMBL:ENSG00000117480"
                               "ENSEMBL:ENSG00000166147"
                               "ENSEMBL:ENSG00000151422"
                               "ENSEMBL:ENSG00000140285"
                               "ENSEMBL:ENSG00000160867"
                               "ENSEMBL:ENSG00000111206"
                               "ENSEMBL:ENSG00000115414"
                               "ENSEMBL:ENSG00000170345"
                               "ENSEMBL:ENSG00000154727"
                               "ENSEMBL:ENSG00000163288"
                               "ENSEMBL:ENSG00000128683"
                               "ENSEMBL:ENSG00000141448"
                               "ENSEMBL:ENSG00000168621"
                               "ENSEMBL:ENSG00000134812"
                               "ENSEMBL:ENSG00000265107"
                               "ENSEMBL:ENSG00000173221"
                               "ENSEMBL:ENSG00000147437"
                               "ENSEMBL:ENSG00000186810"
                               "ENSEMBL:ENSG00000167701"
                               "ENSEMBL:ENSG00000113580"
                               "ENSEMBL:ENSG00000082701"
                               "ENSEMBL:ENSG00000132518"
                               "ENSEMBL:ENSG00000145649"
                               "ENSEMBL:ENSG00000100453"
                               "ENSEMBL:ENSG00000148702"
                               "ENSEMBL:ENSG00000084754"
                               "ENSEMBL:ENSG00000019991"
                               "ENSEMBL:ENSG00000100644"
                               "ENSEMBL:ENSG00000189403"
                               "ENSEMBL:ENSG00000100292"
                               "ENSEMBL:ENSG00000125798"
                               "ENSEMBL:ENSG00000135486"
                               "ENSEMBL:ENSG00000257017"
                               "ENSEMBL:ENSG00000113905"
                               "ENSEMBL:ENSG00000176387"
                               "ENSEMBL:ENSG00000204389"
                               "ENSEMBL:ENSG00000204388"
                               "ENSEMBL:ENSG00000170606"
                               "ENSEMBL:ENSG00000070614"
                               "ENSEMBL:ENSG00000041982"
                               "ENSEMBL:ENSG00000090339"
                               "ENSEMBL:ENSG00000185745"
                               "ENSEMBL:ENSG00000171855"
                               "ENSEMBL:ENSG00000111537"
                               "ENSEMBL:ENSG00000006652"
                               "ENSEMBL:ENSG00000017427"
                               "ENSEMBL:ENSG00000146674"
                               "ENSEMBL:ENSG00000167779"
                               "ENSEMBL:ENSG00000104365"
                               "ENSEMBL:ENSG00000115008"
                               "ENSEMBL:ENSG00000125538"
                               "ENSEMBL:ENSG00000115594"
                               "ENSEMBL:ENSG00000136689"
                               "ENSEMBL:ENSG00000109471"
                               "ENSEMBL:ENSG00000134460"
                               "ENSEMBL:ENSG00000113520"
                               "ENSEMBL:ENSG00000136244"
                               "ENSEMBL:ENSG00000168685"
                               "ENSEMBL:ENSG00000169429"
                               "ENSEMBL:ENSG00000136634"
                               "ENSEMBL:ENSG00000169194"
                               "ENSEMBL:ENSG00000112115"
                               "ENSEMBL:ENSG00000150782"
                               "ENSEMBL:ENSG00000169245"
                               "ENSEMBL:ENSG00000125347"
                               "ENSEMBL:ENSG00000005884"
                               "ENSEMBL:ENSG00000169896"
                               "ENSEMBL:ENSG00000160255"
                               "ENSEMBL:ENSG00000115474"
                               "ENSEMBL:ENSG00000128052"
                               "ENSEMBL:ENSG00000171345"
                               "ENSEMBL:ENSG00000172037"
                               "ENSEMBL:ENSG00000148346"
                               "ENSEMBL:ENSG00000115850"
                               "ENSEMBL:ENSG00000131981"
                               "ENSEMBL:ENSG00000138039"
                               "ENSEMBL:ENSG00000105370"
                               "ENSEMBL:ENSG00000226979"
                               "ENSEMBL:ENSG00000160932"
                               "ENSEMBL:ENSG00000183918"
                               "ENSEMBL:ENSG00000277443"
                               "ENSEMBL:ENSG00000166949"
                               "ENSEMBL:ENSG00000165471"
                               "ENSEMBL:ENSG00000143384"
                               "ENSEMBL:ENSG00000014641"
                               "ENSEMBL:ENSG00000110492"
                               "ENSEMBL:ENSG00000095015"
                               "ENSEMBL:ENSG00000240972"
                               "ENSEMBL:ENSG00000138755"
                               "ENSEMBL:ENSG00000087245"
                               "ENSEMBL:ENSG00000149968"
                               "ENSEMBL:ENSG00000100985"
                               "ENSEMBL:ENSG00000005381"
                               "ENSEMBL:ENSG00000130830"
                               "ENSEMBL:ENSG00000125148"
                               "ENSEMBL:ENSG00000087250"
                               "ENSEMBL:ENSG00000171100"
                               "ENSEMBL:ENSG00000210195"
                               "ENSEMBL:ENSG00000185499"
                               "ENSEMBL:ENSG00000215182"
                               "ENSEMBL:ENSG00000136997"
                               "ENSEMBL:ENSG00000172936"
                               "ENSEMBL:ENSG00000109063"
                               "ENSEMBL:ENSG00000065534"
                               "ENSEMBL:ENSG00000116044"
                               "ENSEMBL:ENSG00000109320"
                               "ENSEMBL:ENSG00000100906"
                               "ENSEMBL:ENSG00000001167"
                               "ENSEMBL:ENSG00000089250"
                               "ENSEMBL:ENSG00000164867"
                               "ENSEMBL:ENSG00000161270"
                               "ENSEMBL:ENSG00000135318"
                               "ENSEMBL:ENSG00000111331"
                               "ENSEMBL:ENSG00000112038"
                               "ENSEMBL:ENSG00000089041"
                               "ENSEMBL:ENSG00000185624"
                               "ENSEMBL:ENSG00000007168"
                               "ENSEMBL:ENSG00000117450"
                               "ENSEMBL:ENSG00000106366"
                               "ENSEMBL:ENSG00000126759"
                               "ENSEMBL:ENSG00000197249"
                               "ENSEMBL:ENSG00000124102"
                               "ENSEMBL:ENSG00000121879"
                               "ENSEMBL:ENSG00000051382"
                               "ENSEMBL:ENSG00000171608"
                               "ENSEMBL:ENSG00000105851"
                               "ENSEMBL:ENSG00000170890"
                               "ENSEMBL:ENSG00000188257"
                               "ENSEMBL:ENSG00000116711"
                               "ENSEMBL:ENSG00000118495"
                               "ENSEMBL:ENSG00000011422"
                               "ENSEMBL:ENSG00000115896"
                               "ENSEMBL:ENSG00000075651"
                               "ENSEMBL:ENSG00000178209"
                               "ENSEMBL:ENSG00000266964"
                               "ENSEMBL:ENSG00000115138"
                               "ENSEMBL:ENSG00000186951"
                               "ENSEMBL:ENSG00000122862"
                               "ENSEMBL:ENSG00000100030"
                               "ENSEMBL:ENSG00000107643"
                               "ENSEMBL:ENSG00000169032"
                               "ENSEMBL:ENSG00000115718"
                               "ENSEMBL:ENSG00000184500"
                               "ENSEMBL:ENSG00000189002"
                               "ENSEMBL:ENSG00000135406"
                               "ENSEMBL:ENSG00000041357"
                               "ENSEMBL:ENSG00000197170"
                               "ENSEMBL:ENSG00000011304"
                               "ENSEMBL:ENSG00000073756"
                               "ENSEMBL:ENSG00000169398"
                               "ENSEMBL:ENSG00000105894"
                               "ENSEMBL:ENSG00000081237"
                               "ENSEMBL:ENSG00000113456"
                               "ENSEMBL:ENSG00000069974"
                               "ENSEMBL:ENSG00000080823"
                               "ENSEMBL:ENSG00000112619"
                               "ENSEMBL:ENSG00000173039"
                               "ENSEMBL:ENSG00000143839"
                               "ENSEMBL:ENSG00000102032"
                               "ENSEMBL:ENSG00000163914"
                               "ENSEMBL:ENSG00000067900"
                               "ENSEMBL:ENSG00000149489"
                               "ENSEMBL:ENSG00000196218"
                               "ENSEMBL:ENSG00000196154"
                               "ENSEMBL:ENSG00000163220"
                               "ENSEMBL:ENSG00000031698"
                               "ENSEMBL:ENSG00000111319"
                               "ENSEMBL:ENSG00000108691"
                               "ENSEMBL:ENSG00000108688"
                               "ENSEMBL:ENSG00000163735"
                               "ENSEMBL:ENSG00000110876"
                               "ENSEMBL:ENSG00000168878"
                               "ENSEMBL:ENSG00000168484"
                               "ENSEMBL:ENSG00000133661"
                               "ENSEMBL:ENSG00000064651"
                               "ENSEMBL:ENSG00000197208"
                               "ENSEMBL:ENSG00000166311"
                               "ENSEMBL:ENSG00000075618"
                               "ENSEMBL:ENSG00000125835"
                               "ENSEMBL:ENSG00000142168"
                               "ENSEMBL:ENSG00000112096"
                               "ENSEMBL:ENSG00000109610"
                               "ENSEMBL:ENSG00000125398"
                               "ENSEMBL:ENSG00000100883"
                               "ENSEMBL:ENSG00000168610"
                               "ENSEMBL:ENSG00000126561"
                               "ENSEMBL:ENSG00000173757"
                               "ENSEMBL:ENSG00000087586"
                               "ENSEMBL:ENSG00000067715"
                               "ENSEMBL:ENSG00000231925"
                               "ENSEMBL:ENSG00000006638"
                               "ENSEMBL:ENSG00000118526"
                               "ENSEMBL:ENSG00000120156"
                               "ENSEMBL:ENSG00000164362"
                               "ENSEMBL:ENSG00000003436"
                               "ENSEMBL:ENSG00000105329"
                               "ENSEMBL:ENSG00000198959"
                               "ENSEMBL:ENSG00000178726"
                               "ENSEMBL:ENSG00000116001"
                               "ENSEMBL:ENSG00000102265"
                               "ENSEMBL:ENSG00000136352"
                               "ENSEMBL:ENSG00000137462"
                               "ENSEMBL:ENSG00000164342"
                               "ENSEMBL:ENSG00000136869"
                               "ENSEMBL:ENSG00000127324"
                               "ENSEMBL:ENSG00000149809"
                               "ENSEMBL:ENSG00000232810"
                               "ENSEMBL:ENSG00000118503"
                               "ENSEMBL:ENSG00000067182"
                               "ENSEMBL:ENSG00000118194"
                               "ENSEMBL:ENSG00000131747"
                               "ENSEMBL:ENSG00000111669"
                               "ENSEMBL:ENSG00000128311"
                               "ENSEMBL:ENSG00000125482"
                               "ENSEMBL:ENSG00000155657"
                               "ENSEMBL:ENSG00000136810"
                               "ENSEMBL:ENSG00000198431"
                               "ENSEMBL:ENSG00000149021"
                               "ENSEMBL:ENSG00000174607"
                               "ENSEMBL:ENSG00000111424"
                               "ENSEMBL:ENSG00000112715"
                               "ENSEMBL:ENSG00000146469"
                               "ENSEMBL:ENSG00000110799"
                               "ENSEMBL:ENSG00000158125"
                               "ENSEMBL:ENSG00000164924"
                               "ENSEMBL:ENSG00000121966"
                               "ENSEMBL:ENSG00000146070"
                               "ENSEMBL:ENSG00000106305"
                               "ENSEMBL:ENSG00000050327"
                               "ENSEMBL:ENSG00000083168"
                               "ENSEMBL:ENSG00000118972"
                               "ENSEMBL:ENSG00000206561"
                               "ENSEMBL:ENSG00000184381"
                               "ENSEMBL:ENSG00000069764"
                               "ENSEMBL:ENSG00000108528"
                               "ENSEMBL:ENSG00000117461"
                               "ENSEMBL:ENSG00000134107"
                               "ENSEMBL:ENSG00000136908"
                               "ENSEMBL:ENSG00000171720"
                               "ENSEMBL:ENSG00000176170"
                               "ENSEMBL:ENSG00000111602"
                               "ENSEMBL:ENSG00000173805"
                               "ENSEMBL:ENSG00000131023"
                               "ENSEMBL:ENSG00000157456"
                               "ENSEMBL:ENSG00000162889"
                               "ENSEMBL:ENSG00000103671"
                               "ENSEMBL:ENSG00000133116"
                               "ENSEMBL:ENSG00000181092"
                               "ENSEMBL:ENSG00000100351"
                               "ENSEMBL:ENSG00000136156"
                               "ENSEMBL:ENSG00000102230"
                               "ENSEMBL:ENSG00000057663"
                               "ENSEMBL:ENSG00000134318"
                               "ENSEMBL:ENSG00000117592"
                               "ENSEMBL:ENSG00000187608"
                               "ENSEMBL:ENSG00000103335"
                               "ENSEMBL:ENSG00000165733"
                               "ENSEMBL:ENSG00000079999"
                               "ENSEMBL:ENSG00000044090"
                               "ENSEMBL:ENSG00000117020"
                               "ENSEMBL:ENSG00000105835"
                               "ENSEMBL:ENSG00000105246"
                               "ENSEMBL:ENSG00000131477"
                               "ENSEMBL:ENSG00000127528"
                               "ENSEMBL:ENSG00000167315"
                               "ENSEMBL:ENSG00000140092"
                               "ENSEMBL:ENSG00000101000"
                               "ENSEMBL:ENSG00000100591"
                               "ENSEMBL:ENSG00000160999"
                               "ENSEMBL:ENSG00000162493"
                               "ENSEMBL:ENSG00000048740"
                               "ENSEMBL:ENSG00000173083"
                               "ENSEMBL:ENSG00000196843"
                               "ENSEMBL:ENSG00000126264"
                               "ENSEMBL:ENSG00000137959"
                               "ENSEMBL:ENSG00000269821"
                               "ENSEMBL:ENSG00000083807"
                               "ENSEMBL:ENSG00000167114"
                               "ENSEMBL:ENSG00000129465"
                               "ENSEMBL:ENSG00000136110"
                               "ENSEMBL:ENSG00000256525"
                               "ENSEMBL:ENSG00000142082"
                               "ENSEMBL:ENSG00000096717"
                               "ENSEMBL:ENSG00000136859"
                               "ENSEMBL:ENSG00000172828"
                               "ENSEMBL:ENSG00000123415"
                               "ENSEMBL:ENSG00000025800"
                               "ENSEMBL:ENSG00000154589"
                               "ENSEMBL:ENSG00000177663"
                               "ENSEMBL:ENSG00000034677"
                               "ENSEMBL:ENSG00000196141"
                               "ENSEMBL:ENSG00000004142"
                               "ENSEMBL:ENSG00000113249"
                               "ENSEMBL:ENSG00000007952"
                               "ENSEMBL:ENSG00000086232"
                               "ENSEMBL:ENSG00000138744"
                               "ENSEMBL:ENSG00000163106"
                               "ENSEMBL:ENSG00000138375"
                               "ENSEMBL:ENSG00000086991"
                               "ENSEMBL:ENSG00000219430"
                               "ENSEMBL:ENSG00000138303"
                               "ENSEMBL:ENSG00000126524"
                               "ENSEMBL:ENSG00000167772"
                               "ENSEMBL:ENSG00000165682"
                               "ENSEMBL:ENSG00000101916"
                               "ENSEMBL:ENSG00000172458"
                               "ENSEMBL:ENSG00000124731"
                               "ENSEMBL:ENSG00000011426"
                               "ENSEMBL:ENSG00000135766"
                               "ENSEMBL:ENSG00000241635"
                               "ENSEMBL:ENSG00000104835"
                               "ENSEMBL:ENSG00000185480"
                               "ENSEMBL:ENSG00000110075"
                               "ENSEMBL:ENSG00000079691"
                               "ENSEMBL:ENSG00000171109"
                               "ENSEMBL:ENSG00000151849"
                               "ENSEMBL:ENSG00000126970"
                               "ENSEMBL:ENSG00000169241"
                               "ENSEMBL:ENSG00000074842"
                               "ENSEMBL:ENSG00000185115"
                               "ENSEMBL:ENSG00000136688"
                               "ENSEMBL:ENSG00000115350"
                               "ENSEMBL:ENSG00000111666"
                               "ENSEMBL:ENSG00000198074"
                               "ENSEMBL:ENSG00000283122"
                               "ENSEMBL:ENSG00000139946"
                               "ENSEMBL:ENSG00000175482"
                               "ENSEMBL:ENSG00000117569"
                               "ENSEMBL:ENSG00000130234"
                               "ENSEMBL:ENSG00000198026"
                               "ENSEMBL:ENSG00000132429"
                               "ENSEMBL:ENSG00000114745"
                               "ENSEMBL:ENSG00000060237"
                               "ENSEMBL:ENSG00000262246"
                               "ENSEMBL:ENSG00000104518"
                               "ENSEMBL:ENSG00000153395"
                               "ENSEMBL:ENSG00000146094"
                               "ENSEMBL:ENSG00000125779"
                               "ENSEMBL:ENSG00000197496"
                               "ENSEMBL:ENSG00000169612"
                               "ENSEMBL:ENSG00000138496"
                               "ENSEMBL:ENSG00000169962"
                               "ENSEMBL:ENSG00000130363"
                               "ENSEMBL:ENSG00000103510"
                               "ENSEMBL:ENSG00000106125"
                               "ENSEMBL:ENSG00000131653"
                               "ENSEMBL:ENSG00000145794"
                               "ENSEMBL:ENSG00000163702"
                               "ENSEMBL:ENSG00000100410"
                               "ENSEMBL:ENSG00000137691"
                               "ENSEMBL:ENSG00000160188"
                               "ENSEMBL:ENSG00000151148"
                               "ENSEMBL:ENSG00000137033"
                               "ENSEMBL:ENSG00000178473"
                               "ENSEMBL:ENSG00000162711"
                               "ENSEMBL:ENSG00000161544"
                               "ENSEMBL:ENSG00000153391"
                               "ENSEMBL:ENSG00000205359"
                               "ENSEMBL:ENSG00000168724"
                               "ENSEMBL:ENSG00000229140"
                               "ENSEMBL:ENSG00000165953"
                               "ENSEMBL:ENSG00000172967"
                               "ENSEMBL:ENSG00000163803"
                               "ENSEMBL:ENSG00000197272"
                               "ENSEMBL:ENSG00000175311"
                               "ENSEMBL:ENSG00000170782"
                               "ENSEMBL:ENSG00000189195"
                               "ENSEMBL:ENSG00000161911"
                               "ENSEMBL:ENSG00000197448"
                               "ENSEMBL:ENSG00000251562"
                               "ENSEMBL:ENSG00000187258"
                               "ENSEMBL:ENSG00000248131"
                               "ENSEMBL:ENSG00000284520"
                               "ENSEMBL:ENSG00000199133"
                               "ENSEMBL:ENSG00000284440"
                               "ENSEMBL:ENSG00000199161"
                               "ENSEMBL:ENSG00000207608"
                               "ENSEMBL:ENSG00000207625"
                               "ENSEMBL:ENSG00000207782"
                               "ENSEMBL:ENSG00000283904"
                               "ENSEMBL:ENSG00000283815"
                               "ENSEMBL:ENSG00000284204"
                               "ENSEMBL:ENSG00000207607"
                               "ENSEMBL:ENSG00000199038"
                               "ENSEMBL:ENSG00000207702"
                               "ENSEMBL:ENSG00000207590"
                               "ENSEMBL:ENSG00000207798"
                               "ENSEMBL:ENSG00000207870"
                               "ENSEMBL:ENSG00000284567"
                               "ENSEMBL:ENSG00000199075"
                               "ENSEMBL:ENSG00000199121"
                               "ENSEMBL:ENSG00000207808"
                               "ENSEMBL:ENSG00000207864"
                               "ENSEMBL:ENSG00000207827"
                               "ENSEMBL:ENSG00000207582"
                               "ENSEMBL:ENSG00000284357"
                               "ENSEMBL:ENSG00000207638"
                               "ENSEMBL:ENSG00000199059"
                               "ENSEMBL:ENSG00000199151"
                               "ENSEMBL:ENSG00000199104"
                               "ENSEMBL:ENSG00000198982"
                               "ENSEMBL:ENSG00000199020"
                               "ENSEMBL:ENSG00000208001"
                               "ENSEMBL:ENSG00000202569"
                               "ENSEMBL:ENSG00000194717"
                               "ENSEMBL:ENSG00000207731"
                               "ENSEMBL:ENSG00000122852"
                               "ENSEMBL:ENSG00000207571"
                               "ENSEMBL:ENSG00000185303"
                               "ENSEMBL:ENSG00000211590"
                               "ENSEMBL:ENSG00000285427"
                               "ENSEMBL:ENSG00000228750"
                               "ENSEMBL:ENSG00000280634"
                               "ENSEMBL:ENSG00000243438"
                               "ENSEMBL:ENSG00000201796"
                               "ENSEMBL:ENSG00000240160")))

(define (unwrap lst)
  (if (null? lst) lst
      (append (car lst) (unwrap (cdr lst)))))

(define (predicate e)
  (cdr (list-ref e 4)))

(define (summary-obj edge-sum)
  (car (list-ref edge-sum 4)))

(define (summary-subj edge-sum)
  (car (list-ref edge-sum 3)))

(define ((edge/db? db) e) (eq? db (car e)))

(define (sort-inner-hash h)
  (sort
   (hash-map h (lambda (k v) (cons k v)))
   >
   #:key (lambda (x) (set-count (cdr x)))))

(define (sort-outer-hash h)
  (sort
   (hash-map h (lambda (k v) (cons k v)))
   >
   #:key (lambda (x) (length (cdr x)))))

(define (sort-counter h)
  (sort
   (hash-map h (lambda (k v) (cons k v)))
   >
   #:key (lambda (x) (cdr x))))

#|getting uniprots using run/graph
(define encodes (find-predicates '("encodes")))
(define gene-curie?
  (lambda (x)
    (or
     (string-prefix? x "HGNC:")
     (string-prefix? x "ENSEMBL:")
     (string-prefix? x "NCBIGene:"))))

(define encodes-us (remove-duplicates (unwrap (filter (lambda (x) (not (null? x))) (set-map gene-list
                                                                         (lambda (g) (time
                                                                                      (define S (filter (lambda (x) (eq? (car x) 'rtx2)) (find-concepts #t (filter gene-curie? (map car (curie-synonyms/names g))))))
                                                                                      (match-define
                                                                                        (list name=>concepts name=>edges)
                                                                                        (run/graph
                                                                                         ((G S)
                                                                                          (U #f))
                                                                                         ((G->U encodes))
                                                                                         (G G->U U)))
                                                                                      (map concept->curie (hash-ref name=>concepts 'U))
                                                                                      ))
                                                                         )))))
|#

(define uniprot-curie?
  (lambda (x)
    (string-prefix? x "UniProtKB:")))

;;Converting ENSEMBL curies to UniProtKB curies using synonymization in order to pass into count-downstream
(define uniprots (remove-duplicates (filter uniprot-curie? (map car (unwrap (set-map gene-list curie-synonyms/names))))))

#|
takes: list of UniProt curies (prot-list), list of GO predicates** (preds)
** note: preds should only contain "positively_regulates", "negatively_regulates", "subclass_of", or any combination
**       of those three predicates depending on what type of relationshion you want to count

returns: a list of the hash tables in this order: go-processes, go-process-members, cell-expression-hash, regulated-cell-expression-hash
         The contents of these hashes are documented inside the function
|#
(define (count-downstream prot-list preds)
  ;;maps each Uniprot curie (key) in prot-list -> a set of all GO pathways (value) that the UniProt (key) regulates
  (define go-processes (make-hash))
  ;;maps GO pathway curies (key) -> a set of all UniProts (value) that are members of the GO pathway (key) (not intersected with prot-list)
  (define go-process-members (make-hash))
  ;;maps each Uniprot in prot-list (key) -> a set of cell/tissue types (value) they're expressed in
  (define cell-expression-hash (make-hash))
  ;;maps each regulated gene (key) found from the 2-hop query -> a set of cell/tissue types (value) they're expressed in
  (define regulated-genes-cell-expression-hash (make-hash))
  (define involved_in (keep 1 (filter (lambda (x) (equal? (car x) 'rtx2)) (find-exact-predicates '("involved_in")))))
  (define regulators (filter (lambda (x) (equal? (car x) 'rtx2)) (find-exact-predicates preds)))
  (define expressed_in (filter (lambda (x) (equal? (car x) 'rtx2)) (find-exact-predicates '("expressed_in"))))
  (for-each
   (lambda (u)
     (define S (filter (lambda (x) (equal? (car x) 'rtx2)) (find-concepts #t (list u))))
     ;;G->M->X 1.1 + 1.3
     ;;G->C - 1.2
     ;;rG->X - 1.4
     ;;rG->rC - 1.5
     (unless (empty? S)
       (match-define
         (list name=>concepts name=>edges)
         (time (run/graph
                ((G S)
                 (M #f)
                 (X #f)
                 (C #f)
                 (rG #f)
                 (rC #f))
                ((G->M involved_in)
                 (G->C expressed_in)
                 (M->X regulators)
                 (rG->X involved_in)
                 (rG->rC expressed_in))
                (G G->M M)
                (G G->C C)
                (M M->X X)
                (rG rG->X X)
                (rG rG->rC rC))))
       ;;go-processes (k=>v): G=>X
       (hash-set! go-processes u (list->set (map concept->curie (hash-ref name=>concepts 'X))))
       ;;cell-expression-hash (k=>v): G=>C
       (hash-set! cell-expression-hash u (list->set (filter (lambda (x) (string-prefix? x "UBERON:")) (map concept->curie (hash-ref name=>concepts 'C)))))
       ;;regulated-genes-cell-expression-hash (k=>v): rG=>rC
       (for-each
        (lambda (e)
          (unless (not (string-prefix? (concept->curie (edge->object e)) "UBERON:"))
            (hash-update! regulated-genes-cell-expression-hash (concept->curie (edge->subject e)) (lambda (v) (set-add v (concept->curie (edge->object e)))) (set))
            )
          )
        (hash-ref name=>edges 'rG->rC)
        )
       ;;go-process-members (k=>v): X=>rG
       (for-each
        (lambda (e)
          (unless (set-member? S (concept->curie (edge->subject e)))
            (hash-update! go-process-members (concept->curie (edge->object e)) (lambda (v) (set-add v (concept->curie (edge->subject e)))) (set))
            )
          )
        (hash-ref name=>edges 'rG->X)
        )
       )
     )
   prot-list
   )
  (list go-processes go-process-members cell-expression-hash regulated-genes-cell-expression-hash)
  )
#|Examples
(define ards-pos-reg (count-downstream uniprots '("positively_regulates")))
(define ards-neg-reg (count-downstream uniprots '("negatively_regulates")))
(define ards-subclass_of (count-downstream uniprots '("subclass_of")))
(define ards-all-regulation (count-downstream uniprots '("positively_regulates" "negatively_regulates" "subclass_of")))
|#


#|
takes: a list of 4 hash tables returned by the count-downstream function above

Manipulates the given hash tables to provide a ranked list of the genes of interest (given as argument prot-list in count-downstream function) by the number of
other genes they regulate within each cell/tissue type thet they're expressed in. Also gives a ranked list of the cell/tissue types by the number of the genes
of interest that are expressed in them. This is to make finding possible cells/tissues of interest easier.

returns: A list of 2 assoc lists - overview of the structure: (list
                                                                (list (C' . int)
                                                                      (C" . int)
                                                                        ...
                                                                  )
                                                                (list (C' (G . (set rG's))
                                                                          (G . (set rG's))
                                                                           ...
                                                                        )
                                                                      (C" (G . (set rG's))
                                                                          (G . (set rG's))
                                                                           ...
                                                                        )
                                                                       ...
                                                                  )
                                                                )
The first assoc list (key . value): the key is the cell/tissue type (C), the value is (int) the number of genes of interest that are expressed in that
                                    cell/tissue type. The first assoc is sorted from highest to lowest int
The second assoc list (key . (key . value)): the first key is the cell/tissue type (C), the value is another assoc list where the keys are the genes of interest (G)
  (a nested assoc list)                      expressed in the cell/tissue type (C) and the values are sets of other genes (rG) that are expressed in the cell/tissue type (C)
                                             AND regulated by the gene of interest (G)
                                             The inner assoc list is sorted (high->low) by the number of regulated genes (rG). The outer assoc list is given in the same order as
                                             the first assoc list above.
|#
(define (count-by-cell-expressed-in count-downstream-results)
  (define G=>X (list-ref count-downstream-results 0))
  (define X=>rG (list-ref count-downstream-results 1))
  (define G=>C (list-ref count-downstream-results 2))
  (define rG=>rC (list-ref count-downstream-results 3))
  (define cell-expression-ranks (make-hash))
  (define counter (make-hash))
  ;;populate cell-expression-ranks
  (hash-for-each
   G=>X
   (lambda (G X)
     (set-for-each
      X
      (lambda (x)
        (set-for-each
         (set-intersect (hash-ref X=>rG x (set)) (list->set uniprots));;filters for just ARDS genes, remove set-intersect for no filter
         (lambda (rg)
           (set-for-each
            (set-intersect (hash-ref G=>C G (set)) (hash-ref rG=>rC rg (set)))
            (lambda (c)
              (hash-update! cell-expression-ranks c
                            (lambda (h)
                              (hash-update! h G (lambda (v) (set-add v rg)) (set))
                              h
                              )
                            (make-hash)
                            )
              )
            )
           )
         )
        )
      )
     )
   )
  (hash-for-each cell-expression-ranks
                 (lambda (k v)
                   (hash-set! cell-expression-ranks k (sort-inner-hash v))
                   (hash-set! counter k (hash-count v))
                   )
                 )
  (list (sort-counter counter) (sort-outer-hash cell-expression-ranks))
  )
#|Examples
(define ards-cell-type-count (count-by-cell-expressed-in ards-pos-reg))

(car ards-cell-type-count) ;;gives the ranked count of the number of genes expressed in each cell type
(cadr ards-cell-type-count) ;;gives the second (more meaningful) assoc list

;;To get the ranked assoc list of regulating genes for the liver ("UBERON:0002107")
(define liver-genes (cdr (assoc "UBERON:0002107" (cadr ards-cell-type-count))))
;;for the lung ("UBERON:0002048")
(define lung-genes (cdr (assoc "UBERON:0002048" (cadr ards-cell-type-count))))
|#

#|
(make-hash 'ARDS-Genelist
           (make-hash 'Gene1-ARDS
                      (make-hash 'Gene1-UBERON-cell-location
                                 (make-hash 'pos-reg-GO1
                                            (make-hash 'GO1
                                                       (make-hash 'GO1-members
                                                                  (make-hash 'GO1-member-UBERON-Cell-locations
                                                                             (make-hash 'GO1-ARDS-members)
                                                                             )))))))
|#