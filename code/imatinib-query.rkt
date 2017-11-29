#lang racket
;; We are using Racket as our base language.

;; Load the mediKanren logic engine, and the Racket-formatted version
;; of a lightly curated and lightly normalized version of the SemMedDB
;; knowledge base.
(require
  "mk-db.rkt"
  "concept.rkt"
  "edge.rkt"
  )

(displayln
  "Finished loading mk-db.rkt.")

;; We are going to write a query that tries to find a disease 'Ds'
;; that a given drug 'Dg' might treat, but for which SemMedDB doesn't
;; contain a direct TREATS edge between 'Dg' and 'Ds'.  That is, we
;; are trying to find "surprising" drug/disease links that might be
;; worth further exploration.
;;
;; THE BIGGER PROBLEM
;;
;; One important question this example query is meant to explore is
;; whether we can avoid the combinatorial, exponential explosion of
;; possible paths between the given drug 'Dg' and the "unknown"
;; disease 'Ds'.  SemMedDB has millions of edges, 25,000 diseases,
;; thousands of drugs, etc.  If we are naive, we can easily end
;; generating millions of paths between the given drug 'Dg' and the
;; tens of thousands of unknown diseases.  We might think of this as
;; the "Six Degrees of Kevin Bacon" problem
;; (https://en.wikipedia.org/wiki/Six_Degrees_of_Kevin_Bacon): from
;; any disease there may be thousands, or even hundreds of thousands,
;; of indirect paths bewteen that drug and and specific disease.  Not
;; only does this phenomenon lead to computationally intractable
;; problems, it also can overwhelm the human user with far too many
;; answers to consider in a lifetime, most of which are of extremely
;; dubious quality.  For example, there are over 180,000 (relatively
;; short!) paths just between imatinib and asthma in SemMedDB.
;;
;; To avoid this problem, we are going to try to be clever, in
;; multiple dimensions.  We are going to use various forms of
;; reasoning, including trying to explore connections between
;; concepts, exploring more specific drugs before exploring classes of
;; drugs, throwing out "degenrate" answers like 'Disease' that are so
;; general they are guaranteed to lead to exponential explosion, and
;; any other techniques that seem likely to avoid combinatorial
;; explosion in a wide class of queries, without being so aggressive
;; that useful answers are throw out.
;;
;; Philosophically, this is similar to how a chess engine like
;; Stockfish works (https://en.wikipedia.org/wiki/Stockfish_(chess)).
;; Even though the average branching factor in chess is around 35, the
;; best engines use heuristics to reduce the *effective* branching
;; factor to under 2.  That is, when trying to decide on the next move
;; to play, in a chess position with 35 possible moves, most of the
;; time Stockfish will consider only the most promising one or two
;; moves.  This allows Stockfish to efficiently search much deeper
;; than chess engines that consider all 35 moves in detail.
;;
;; In this example we try to explore a few possible heuristics that
;; might let us avoid a high average branching factor, by avoiding
;; paths that semantically don't make sense, avoiding degenerate
;; entities like 'Disease', trying more specific variants of a class
;; of drugs first to reduce the branching factor, etc.
;;
;;
;; THE QUERY
;;
;; By the very nature of our specific query (trying to find surprising
;; candidate diseases that might be treated by a given drug), we know
;; that SemMedDB will not contain the edge:
;;
;;      Dg TREATS Ds
;;
;; (In fact, we will *insist* that such an edge doesn't exist, for Ds
;; to be considered a valid answer.)  So instead of looking for a
;; direct TREATS edge, we will try a more nuanced query:
;;
;; Find an "unknown" disease 'Ds' that is not known to be directly treated by the
;; given drug 'Dg', where there exists a gene 'Gn', a "known" disease
;; 'Ds_k', a cell function 'Cf' such that:
;;
;; The given drug Dg directly INHIBITS some gene Gn, *and*
;; the inhibited gene Gn directly CAUSES some "known" disease Ds_k, *and*
;; the given drug Dg directly TREATS the "known" disease Ds_k, *and*
;; the inhibited gene Gn directly CAUSES some cell function Cf, *and*
;; the cell function Cf directly AFFECTS some "unknown" disease Ds, *and*
;; the given drug Dg does *not* directly TREATS the "unknown" disease Ds, *and*
;; the "unknown" disease Ds directly is a MANIFESTATION_OF the cell function Cf.
;;
;; Upper-case verbs, such as INHIBITS and TREATS, are SemMedDB edge
;; "predicates".  Only the drug 'Dg' is given ("ground" in logic
;; programming terminology) in the query above.  All other entities
;; ('Gn', 'Ds_k', '', etc.) are unknown ("fresh" or "non-ground" in
;; logic programming terminology).  Even the "known" disease 'Ds_k'
;; starts out unknown.  'Ds_k' is a "known" disease only in the sense
;; that SemMedDB contains a direct TREATS edge between the given drug
;; 'Dg' and 'Ds_k'--we must discover that edge, and the disease
;; 'Ds_k', during the query.
;;
;; By parameterizing over the given drug 'Dg', we can turn this
;; specific query about imatinic into a more general "query schema"
;; that can be made to work for any drug.
;;
;; Part of the domain knowledge of a human user is encoded in the
;; *structure* of the query itself.  This structure includes:
;;
;; * which entities are connected to each other;
;;
;; * the exact predicates chosen ('CAUSES' is a "stronger", or more
;; specific, predicate than 'AFFECTS', which is stronger than
;; 'ASSOCIATED_WITH'),
;;
;; * the exact SemMedDB semantic types chosen--diseases of interest
;; may or may not include neoplasms (semantic type 'neop'), for
;; example.
;;
;;
;; THE FIGURES
;;
;; We found it helpful to ourselves, to keep everything straight, to
;; draw by hand several figures showing the simple/naive query, and
;; a more sophisticated version that involves reasoning and heuristics.
;;
;; The overview of a naive version of the query can be found at the top of
;; 
;;   imatinib_simple.jpg
;;
;; This is really only part of a naive query, showing the path we hope
;; to find connecting imatinib to asthma via the KIT gene and mast
;; cell activation.  Since there are over 180,000 relatively short
;; paths paths between imatinib and asthma, this path can be difficult
;; to find, even if you already knew to look for the connection to
;; asthma.  If you didn't know which of the 25,000 diseases in
;; SemMedDB might be connected to imatinib, this query would be very
;; difficult indeed.
;;
;; In the same file is an abstracted version of the query, showing the
;; abstract categories and connections we are interested in.  In this
;; case, we have also abstracted over the drug of interest: since none
;; of the entities are ground/concrete, this is a query schema that
;; could be applied to any drug.  Of course, it is unlikely to give
;; useful answers without significant human intervention to help prune
;; bogus and boring answers.
;;
;; A more sophisticated version of the query can be found in
;;
;;   imatinib_less_naive.jpg
;;
;; This figure shows branching factors we encountered when building up
;; the query by hand, while using various sorts of reasoning and
;; heuristics.  Unlike the naive queries, these sub-queries were
;; executed on a laptop within a few seconds to a few minutes, with
;; relatively small numbers of interesting answers produced at the end
;; (~111 diseases to consider by a human).  We aren't doing too much to
;; be efficient at this point--for example, we are representing sets as
;; lists, and taking other shortcuts.  With some cleverness, we think
;; we could both speed up the queries, and also cut down on the number of
;; bogus/uninteresting answers.
;;
;;
;; CHEATING, BUT NOT REALLY
;;
;; In this file we are hand-compiling parts of the overall query, and
;; using various heuristics and reasoning techniques.  We are most
;; interested in exploring *where we might get leverage to avoid
;; combinatorial explosion* while still keeping answers that are of
;; interest.  In this file we will shamelessly hand-compile or chain
;; together parts of the query, and wave our hands to point out how we
;; applied a certain optimization or bit of reasoning.  Obviously in a
;; real system a human would interact with the reasoning engine at a
;; much higher level of abstraction, and this code would be
;; automatically generated.  Also, we would need to implement the
;; reasoning in a more automatic fashion, and might want to tweak or
;; change some of the hueristics, call out to external ontologies like
;; SNOMED so we can do logical queries to determine how concepts are
;; related, etc.  We have tried to only include reasoning that we
;; think is generalizable, and which you might reasonably want to
;; apply for real queries.
;;
;; Obviously we are starting from an inherently cheating position,
;; since we know there is a connection bewteen imatinib and asthma.
;; However, we will try to be as honest as possible, and not use
;; tricks that just show asthma as the answer (ta da!).  Instead, we
;; want to make sure that each step we take, and each bit of reasoning
;; we employ, seems reasonable.  Over time we will learn which types
;; of reasoning seem to work best in practice--we might not use all of
;; these specific reasoning techniques, but we will use techniques
;; that are similar in spirit.
;;
;;
;; MUCH MORE EXPLORATION
;;
;; can be found in the 'study-imatinib.rkt' file, which is basically
;; 10K lines of queries and answers exploring different possible
;; answers, approaches to reasoning, etc.  The examples and code below
;; have been taken from 'study-imatinib.rkt'.


;; Racket and mediKanren helpers

;; remove duplicates from a list
(define rem-dups
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(member (car ls) (cdr ls)) (rem-dups (cdr ls))]
      [else (cons (car ls) (rem-dups (cdr ls)))])))

;; subtract elements from set l2 from set l1 (represented as lists)
(define set-subtraction
  (lambda (l1 l2)
    (cond
      [(null? l1) '()]
      [(member (car l1) l2) (set-subtraction (cdr l1) l2)]
      [else (cons (car l1) (set-subtraction (cdr l1) l2))])))

;; set union
(define union
  (lambda (l1 l2)
    (cond
      [(null? l1) l2]
      [(member (car l1) l2) (union (cdr l1) l2)]
      [else (cons (car l1) (union (cdr l1) l2))])))

;; set union for any number of sets
(define union*
  (lambda args
    (union*-aux args)))

;; helper for union*
(define union*-aux
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(null? (cdr ls)) (car ls)]
      [else (union (car ls) (union*-aux (cdr ls)))])))


;; list membership helpers for mediKanren (depricated)
(define membero
  (lambda (x ls)
    (fresh (y rest)
      (== `(,y . ,rest) ls)
      (conde
        [(== x y)]
        [(=/= x y) (membero x rest)]))))

(define not-membero
  (lambda (x ls)
    (conde
      [(== '() ls)]
      [(fresh (y rest)
         (== `(,y . ,rest) ls)
         (=/= x y)
         (not-membero x rest))])))


;; THE CANDIDATE DISEASES
;;
;; Working backwards, here we calculate the final 111 candidate
;; diseases.
;;
;; subtract from the 154 disorders (no neoplasms or pathologic functions)
;; of interest those 456 disorders, neoplasms, or pathologic functions
;; directly treated by *any* form of imatinib (not just Gleevec)
;;
;; results in 111 disorders
;;
;; Should be able to pare down more using ontologies to see which
;; entries are forms of diabetes, for example.  And use ontologies to
;; group remaning entries.
(define *final-disease-candidates*
  (set-subtraction
   '((10054 "Coronary Arteriosclerosis" ("dsyn"))
     (41107 "Trisomy" ("dsyn"))
     (25517 "Metabolic Diseases" ("dsyn"))
     (15695 "Fatty Liver" ("dsyn"))
     (752304 "Hypoxic-Ischemic Encephalopathy" ("dsyn"))
     (17732 "Glucose Intolerance" ("dsyn"))
     (158981 "Neonatal diabetes mellitus" ("dsyn"))
     (6267 "Bronchiectasis" ("dsyn"))
     (11616 "Contact Dermatitis" ("dsyn"))
     (32285 "Pneumonia" ("dsyn"))
     (1519680 "Tumor Immunity" ("dsyn"))
     (242231 "Coronary Stenosis" ("dsyn"))
     (729353 "Subfertility" ("dsyn"))
     (9447 "Common Variable Immunodeficiency" ("dsyn"))
     (33860 "Psoriasis" ("dsyn"))
     (30920 "Peptic Ulcer" ("dsyn"))
     (87086 "Thrombus" ("dsyn"))
     (339510 "Vitelliform dystrophy" ("dsyn"))
     (1857 "AIDS related complex" ("dsyn"))
     (14038 "Encephalitis" ("dsyn"))
     (35334 "Retinitis Pigmentosa" ("dsyn"))
     (19163 "Hepatitis B" ("dsyn"))
     (35435 "Rheumatism" ("dsyn"))
     (38525 "Subarachnoid Hemorrhage" ("dsyn"))
     (221757 "alpha 1-Antitrypsin Deficiency" ("dsyn"))
     (948089 "Acute coronary syndrome" ("dsyn"))
     (231341 "Premature aging syndrome" ("dsyn"))
     (14553 "Absence Epilepsy" ("dsyn"))
     (19151 "Hepatic Encephalopathy" ("dsyn"))
     (20437 "Hypercalcemia" ("dsyn"))
     (24899 "mastocytosis" ("dsyn"))
     (178664 "Glomerulosclerosis" ("dsyn"))
     (4153 "Atherosclerosis" ("dsyn"))
     (4623 "Bacterial Infections" ("dsyn"))
     (15397 "Eye diseases" ("dsyn"))
     (21051 "Immunologic Deficiency Syndromes" ("dsyn"))
     (26848 "Myopathy" ("dsyn"))
     (35304 "Retinal Degeneration" ("dsyn"))
     (35309 "Retinal Diseases" ("dsyn"))
     (38220 "Status Epilepticus" ("dsyn"))
     (85084 "Motor Neuron Disease" ("dsyn"))
     (339573 "Primary open angle glaucoma" ("dsyn"))
     (1285162 "Degenerative disorder" ("dsyn"))
     (1290884 "Inflammatory disorder" ("dsyn"))
     (1536085 "Geographic atrophy" ("dsyn"))
     (18133 "Graft-vs-Host Disease" ("dsyn"))
     (20459 "Hyperinsulinism" ("dsyn"))
     (9319 "Colitis" ("dsyn"))
     (11881 "Diabetic Nephropathy" ("dsyn"))
     (14544 "Epilepsy" ("dsyn"))
     (17601 "Glaucoma" ("dsyn"))
     (19158 "Hepatitis" ("dsyn"))
     (20456 "Hyperglycemia" ("dsyn"))
     (20538 "Hypertensive disease" ("dsyn"))
     (20550 "Hyperthyroidism" ("dsyn"))
     (20615 "hypoglycemia" ("dsyn"))
     (24141 "Lupus Erythematosus, Systemic" ("dsyn"))
     (30305 "Pancreatitis" ("dsyn"))
     (33626 "Protein Deficiency" ("dsyn"))
     (36421 "Systemic Scleroderma" ("dsyn"))
     (38454 "Cerebrovascular accident" ("dsyn"))
     (151747 "Renal tubular disorder" ("dsyn"))
     (239946 "Fibrosis, Liver" ("dsyn"))
     (270814 "Spastic syndrome" ("dsyn"))
     (400966 "Non-alcoholic fatty liver" ("dsyn"))
     (878544 "Cardiomyopathies" ("dsyn"))
     (948008 "Ischemic stroke" ("dsyn"))
     (1175 "Acquired Immunodeficiency Syndrome" ("dsyn"))
     (1824 "Agranulocytosis" ("dsyn"))
     (2395 "Alzheimer's Disease" ("dsyn"))
     (2736 "Amyotrophic Lateral Sclerosis" ("dsyn"))
     (2871 "Anemia" ("dsyn"))
     (3873 "Rheumatoid Arthritis" ("dsyn"))
     (4135 "Ataxia Telangiectasia" ("dsyn"))
     (4364 "Autoimmune Diseases" ("dsyn"))
     (7193 "Cardiomyopathy, Dilated" ("dsyn"))
     (7222 "Cardiovascular Diseases" ("dsyn"))
     (7785 "Cerebral Infarction" ("dsyn"))
     (8312 "Primary biliary cirrhosis" ("dsyn"))
     (8370 "Cholestasis" ("dsyn"))
     (11615 "Dermatitis, Atopic" ("dsyn"))
     (11847 "Diabetes" ("dsyn"))
     (11849 "Diabetes Mellitus" ("dsyn"))
     (11854 "Diabetes Mellitus, Insulin-Dependent" ("dsyn"))
     (11860 "Diabetes Mellitus, Non-Insulin-Dependent" ("dsyn"))
     (11884 "Diabetic Retinopathy" ("dsyn"))
     (13595 "Eczema" ("dsyn"))
     (14175 "Endometriosis, site unspecified" ("dsyn"))
     (17152 "Gastritis" ("dsyn"))
     (17658 "Glomerulonephritis" ("dsyn"))
     (18799 "Heart Diseases" ("dsyn"))
     (18801 "Heart failure" ("dsyn"))
     (19693 "HIV Infections" ("dsyn"))
     (20179 "Huntington Disease" ("dsyn"))
     (20542 "Hypertension, Pulmonary" ("dsyn"))
     (21053 "Immune System Diseases" ("dsyn"))
     (21311 "Infection" ("dsyn"))
     (21359 "Infertility" ("dsyn"))
     (21364 "Infertility, Male" ("dsyn"))
     (21390 "Inflammatory Bowel Diseases" ("dsyn"))
     (22116 "Ischemia" ("dsyn"))
     (22658 "Kidney Diseases" ("dsyn"))
     (22660 "Kidney Failure, Acute" ("dsyn"))
     (23530 "Leukopenia" ("dsyn"))
     (23895 "Liver diseases" ("dsyn"))
     (24117 "Chronic Obstructive Airway Disease" ("dsyn"))
     (24312 "Lymphopenia" ("dsyn"))
     (26769 "Multiple Sclerosis" ("dsyn"))
     (27051 "Myocardial Infarction" ("dsyn"))
     (27765 "nervous system disorder" ("dsyn"))
     (28754 "Obesity" ("dsyn"))
     (29408 "Degenerative polyarthritis" ("dsyn"))
     (29456 "Osteoporosis" ("dsyn"))
     (30567 "Parkinson Disease" ("dsyn"))
     (31763 "Photosensitization" ("dsyn"))
     (32914 "Pre-Eclampsia" ("dsyn"))
     (35305 "Retinal Detachment" ("dsyn"))
     (36690 "Septicemia" ("dsyn"))
     (38644 "Sudden infant death syndrome" ("dsyn"))
     (39082 "Syndrome" ("dsyn"))
     (40034 "Thrombocytopenia" ("dsyn"))
     (41296 "Tuberculosis" ("dsyn"))
     (42024 "Urinary Incontinence" ("dsyn"))
     (42341 "Varicocele" ("dsyn"))
     (42721 "Viral hepatitis" ("dsyn"))
     (42769 "Virus Diseases" ("dsyn"))
     (86543 "Cataract" ("anab" "dsyn"))
     (151650 "Renal fibrosis" ("dsyn"))
     (151744 "Myocardial Ischemia" ("dsyn"))
     (158266 "Degenerative disc disease NOS" ("dsyn"))
     (162557 "Liver Failure, Acute" ("dsyn"))
     (162871 "Aortic Aneurysm, Abdominal" ("dsyn"))
     (206139 "Lichen Planus, Oral" ("dsyn"))
     (238806 "BONE MASS" ("dsyn"))
     (242350 "Erectile dysfunction" ("dsyn"))
     (242383 "Age related macular degeneration" ("dsyn"))
     (242422 "Parkinsonian Disorders" ("dsyn"))
     (268731 "Renal glomerular disease" ("dsyn"))
     (270994 "Steroid-induced myopathy" ("dsyn"))
     (339527 "Leber's amaurosis" ("dsyn"))
     (340970 "Congenital neutropenia" ("dsyn"))
     (343641 "Human papilloma virus infection" ("dsyn"))
     (456909 "Blind Vision" ("dsyn"))
     (524851 "Neurodegenerative Disorders" ("dsyn"))
     (677607 "Hashimoto Disease" ("dsyn"))
     (856169 "Endothelial dysfunction" ("dsyn"))
     (857357 "Hepatic pathology" ("dsyn"))
     (917798 "Cerebral Ischemia" ("dsyn"))
     (1281300 "Vascular degeneration" ("dsyn"))
     (1456670 "Nerve Diseases" ("dsyn"))
     (4096 "Asthma" ("dsyn"))
     (12634 "Disease" ("dsyn"))
     (22661 "Kidney Failure, Chronic" ("dsyn"))
     (23882 "Little's Disease" ("dsyn")))
   '((2871 "Anemia" ("dsyn"))
     (2874 "Aplastic Anemia" ("dsyn"))
     (2895 "Sickle Cell Anemia" ("dsyn"))
     (3047 "Animal Diseases" ("dsyn"))
     (15376 "Extravasation" ("patf"))
     (5684 "Malignant neoplasm of urinary bladder" ("neop"))
     (4153 "Atherosclerosis" ("dsyn"))
     (5684 "Malignant neoplasm of urinary bladder" ("neop"))
     (5940 "Bone Diseases" ("dsyn"))
     (18944 "Hematoma" ("patf"))
     (7193 "Cardiomyopathy, Dilated" ("dsyn"))
     (7682 "CNS disorder" ("dsyn"))
     (20507 "Hyperplasia" ("patf"))
     (6118 "Brain Neoplasms" ("neop"))
     (21368 "Inflammation" ("patf"))
     (8728 "Churg-Strauss Syndrome" ("dsyn"))
     (6142 "Malignant neoplasm of breast" ("neop"))
     (10403 "Cryoglobulinemia" ("dsyn"))
     (11644 "Scleroderma" ("dsyn"))
     (6142 "Malignant neoplasm of breast" ("neop"))
     (29435 "Osteolysis" ("patf"))
     (11854 "Diabetes Mellitus, Insulin-Dependent" ("dsyn"))
     (36429 "Sclerosis" ("patf"))
     (11881 "Diabetic Nephropathy" ("dsyn"))
     (36974 "Shock" ("patf"))
     (7095 "Carcinoid Tumor" ("neop"))
     (14175 "Endometriosis, site unspecified" ("dsyn"))
     (86565 "Liver Dysfunction" ("patf"))
     (7095 "Carcinoid Tumor" ("neop"))
     (15230 "Exanthema" ("dsyn"))
     (151654 "Myocardial fibrosis" ("patf"))
     (15230 "Exanthema" ("dsyn"))
     (7097 "Carcinoma" ("neop"))
     (15624 "Fanconi Syndrome" ("dsyn"))
     (151746 "Abnormal renal function" ("patf"))
     (17152 "Gastritis" ("dsyn"))
     (7103 "Malignant neoplasm of endometrium" ("neop"))
     (17658 "Glomerulonephritis" ("dsyn"))
     (151746 "Abnormal renal function" ("patf"))
     (7114 "Malignant neoplasm of skin" ("neop"))
     (18801 "Heart failure" ("dsyn"))
     (231178 "Chronic failure" ("patf"))
     (19196 "Hepatitis C" ("dsyn"))
     (20456 "Hyperglycemia" ("dsyn"))
     (20538 "Hypertensive disease" ("dsyn"))
     (21141 "Inappropriate ADH Syndrome" ("dsyn"))
     (21390 "Inflammatory Bowel Diseases" ("dsyn"))
     (22658 "Kidney Diseases" ("dsyn"))
     (23882 "Little's Disease" ("dsyn"))
     (23890 "Liver Cirrhosis" ("dsyn"))
     (9404 "Colorectal Neoplasms" ("neop"))
     (24115 "Lung diseases" ("dsyn"))
     (333606 "Dystrophy" ("patf"))
     (24440 "Macular Edema, Cystoid" ("dsyn"))
     (443146 "Autoimmune" ("patf"))
     (26769 "Multiple Sclerosis" ("dsyn"))
     (27697 "Nephritis" ("dsyn"))
     (549593 "kidney functional" ("patf"))
     (27947 "Neutropenia" ("dsyn"))
     (16048 "Fibromatosis" ("neop"))
     (33838 "Kimura Disease" ("dsyn"))
     (33860 "Psoriasis" ("dsyn"))
     (34063 "Pulmonary Edema" ("dsyn"))
     (744813 "Hepatic embolisation" ("patf"))
     (35309 "Retinal Diseases" ("dsyn"))
     (879626 "Adverse effects" ("patf"))
     (35920 "Rubella" ("dsyn"))
     (879626 "Adverse effects" ("patf"))
     (18923 "Hemangiosarcoma" ("neop"))
     (36992 "Short Bowel Syndrome" ("dsyn"))
     (1265815 "Multiple ulcers" ("patf"))
     (38013 "Ankylosing spondylitis" ("dsyn"))
     (19204 "Primary carcinoma of the liver cells" ("neop"))
     (1608322 "Leak NOS" ("patf"))
     (39103 "Synovitis" ("dsyn"))
     (19204 "Primary carcinoma of the liver cells" ("neop"))
     (41296 "Tuberculosis" ("dsyn"))
     (85786 "Hamman-Rich syndrome" ("dsyn"))
     (23434 "Chronic Lymphocytic Leukemia" ("neop"))
     (86438 "Hypogammaglobulinemia" ("dsyn"))
     (151859 "Polyserositis" ("dsyn"))
     (23448 "Lymphoblastic Leukemia" ("neop"))
     (158168 "Villonodular synovitis" ("dsyn"))
     (162557 "Liver Failure, Acute" ("dsyn"))
     (162557 "Liver Failure, Acute" ("dsyn"))
     (206062 "Lung Diseases, Interstitial" ("dsyn"))
     (206143 "Loeffler's Endocarditis" ("dsyn"))
     (236178 "Intraabdominal hemorrhage" ("dsyn"))
     (238644 "anemia; profound" ("dsyn"))
     (238790 "destruction; bone" ("dsyn"))
     (239946 "Fibrosis, Liver" ("dsyn"))
     (263664 "Generalized morphea" ("dsyn"))
     (264939 "Systemic vasculitis" ("dsyn"))
     (23475 "Leukemia, Myeloid, Philadelphia-Negative" ("neop"))
     (272203 "Indolent Systemic Mastocytosis" ("dsyn"))
     (276653 "Invasive pulmonary aspergillosis" ("dsyn"))
     (277554 "Primary disease" ("dsyn"))
     (277556 "Recurrent disease" ("dsyn"))
     (334102 "Lymphangiomatosis" ("dsyn"))
     (23484 "Leukemia, Plasmacytic" ("neop"))
     (340548 "Pulmonary capillary hemangiomatosis" ("dsyn"))
     (23601 "Leydig Cell Tumor" ("neop"))
     (341213 "External gastric fistula" ("dsyn"))
     (24301 "Lymphoma, Follicular" ("neop"))
     (341439 "Chronic liver disease NOS" ("dsyn"))
     (24623 "Malignant neoplasm of stomach" ("neop"))
     (442867 "Malignant disease" ("dsyn"))
     (549567 "Pigmentation Disorders" ("dsyn"))
     (678236 "Rare Diseases" ("dsyn"))
     (743496 "END ORGAN DAMAGE" ("dsyn"))
     (25286 "meningioma" ("neop"))
     (25500 "Mesothelioma" ("neop"))
     (854467 "Myelosuppression" ("dsyn"))
     (26764 "Multiple Myeloma" ("neop"))
     (855227 "Purging" ("dsyn"))
     (26986 "Dysmyelopoietic Syndromes" ("neop"))
     (856169 "Endothelial dysfunction" ("dsyn"))
     (878544 "Cardiomyopathies" ("dsyn"))
     (920627 "Orphan Diseases" ("dsyn"))
     (948008 "Ischemic stroke" ("dsyn"))
     (948908 "Nephrotoxic serum nephritis" ("dsyn"))
     (1273070 "Left ventricular diastolic dysfunction" ("dsyn"))
     (1290884 "Inflammatory disorder" ("dsyn"))
     (27832 "Neurofibromatosis 2" ("neop"))
     (1299884 "Eosinophilic myositis" ("dsyn"))
     (1306759 "Eosinophilic disorder" ("dsyn"))
     (1306759 "Eosinophilic disorder" ("dsyn"))
     (1332309 "Anti-Basement Membrane Glomerulonephritis" ("dsyn"))
     (1533022 "Histiocytic proliferation" ("dsyn"))
     (1565489 "Renal Insufficiency" ("dsyn"))
     (36221 "Mast-Cell Sarcoma" ("neop"))
     (41341 "Tuberous Sclerosis" ("neop"))
     (79731 "B-Cell Lymphomas" ("neop"))
     (79772 "T-Cell Lymphoma" ("neop"))
     (153633 "Malignant neoplasm of brain" ("neop"))
     (153633 "Malignant neoplasm of brain" ("neop"))
     (162678 "Neurofibromatoses" ("neop"))
     (205853 "Neoplasms, Epithelial" ("neop"))
     (206647 "Dermatofibrosarcoma" ("neop"))
     (206647 "Dermatofibrosarcoma" ("neop"))
     (206657 "Sarcoma, Alveolar Soft Part" ("neop"))
     (206754 "Neuroendocrine Tumors" ("neop"))
     (206754 "Neuroendocrine Tumors" ("neop"))
     (220650 "Metastatic malignant neoplasm to brain" ("neop"))
     (238463 "Papillary thyroid carcinoma" ("neop"))
     (242379 "Malignant neoplasm of lung" ("neop"))
     (278517 "Non-small cell lung cancer recurrent" ("neop"))
     (278695 "recurrent neuroblastoma" ("neop"))
     (278704 "Malignant Childhood Neoplasm" ("neop"))
     (278727 "Small cell lung cancer recurrent" ("neop"))
     (279068 "childhood solid tumor" ("neop"))
     (279087 "recurrent Kaposi's sarcoma" ("neop"))
     (281361 "Adenocarcinoma pancreas" ("neop"))
     (302592 "Cervix carcinoma" ("neop"))
     (302592 "Cervix carcinoma" ("neop"))
     (334410 "Leydig cell tumor, malignant" ("neop"))
     (334695 "Endometrial Stromal Tumors" ("neop"))
     (349636 "Pre B-cell acute lymphoblastic leukemia" ("neop"))
     (553580 "Ewings sarcoma" ("neop"))
     (677865 "Brain stem glioma" ("neop"))
     (677865 "Brain stem glioma" ("neop"))
     (685938 "Malignant neoplasm of gastrointestinal tract" ("neop"))
     (686619 "Secondary malignant neoplasm of lymph node" ("neop"))
     (854850 "Mycosis fungoides refractory" ("neop"))
     (855054 "Fibrosarcoma metastatic" ("neop"))
     (855211 "Seminoma of testis" ("neop"))
     (948380 "Colorectal cancer metastatic" ("neop"))
     (948380 "Colorectal cancer metastatic" ("neop"))
     (1266042 "Chromophobe Renal Cell Carcinoma" ("neop"))
     (1266101 "Thymic epithelial neoplasm" ("neop"))
     (1266119 "Solitary fibrous tumor" ("neop"))
     (1266120 "Solitary fibrous tumor, malignant" ("neop"))
     (1300127 "Perivascular epithelial cell tumor" ("neop"))
     (1306837 "Papillary Renal Cell Carcinoma" ("neop"))
     (1318543 "Tenosynovial giant cell tumor" ("neop"))
     (1319185 "Chiasmal glioma" ("neop"))
     (1326912 "Tumorigenesis" ("neop"))
     (1328504 "Hormone-refractory prostate cancer" ("neop"))
     (1328504 "Hormone-refractory prostate cancer" ("neop"))
     (1332884 "Central Nervous System Leukemia" ("neop"))
     (1333614 "Fibrosarcomatous Dermatofibrosarcoma Protuberans" ("neop"))
     (1334432 "Low Risk Gastrointestinal Stromal Tumor" ("neop"))
     (1335996 "Small Intestinal Gastrointestinal Stromal Tumor" ("neop"))
     (1378050 "Oncocytic Neoplasm" ("neop"))
     (1411997 "Acute biphenotypic leukemia" ("neop"))
     (1512409 "Hepatocarcinogenesis" ("neop"))
     (1524028 "Intraepithelial Neoplasia of the Mouse Mammary Gland" ("neop"))
     (3864 "Arthritis" ("dsyn"))
     (3873 "Rheumatoid Arthritis" ("dsyn"))
     (1418 "Adenocarcinoma" ("neop"))
     (4364 "Autoimmune Diseases" ("dsyn"))
     (6272 "Bronchiolitis Obliterans" ("dsyn"))
     (9782 "Connective Tissue Diseases" ("dsyn"))
     (10828 "Cytopenia" ("patf"))
     (11603 "Dermatitis" ("dsyn"))
     (11633 "Dermatomyositis" ("dsyn"))
     (242656 "Disease Progression" ("patf"))
     (14457 "Eosinophilia" ("dsyn"))
     (14457 "Eosinophilia" ("dsyn"))
     (242656 "Disease Progression" ("patf"))
     (18133 "Graft-vs-Host Disease" ("dsyn"))
     (7102 "Malignant tumor of colon" ("neop"))
     (19618 "Histiocytosis" ("dsyn"))
     (243083 "associated disease" ("patf"))
     (19621 "Histiocytosis, Langerhans-Cell" ("dsyn"))
     (7115 "Malignant neoplasm of thyroid" ("neop"))
     (19624 "Histiocytosis, Non-Langerhans-Cell" ("dsyn"))
     (277785 "Functional disorder" ("patf"))
     (19625 "Sinus histiocytosis" ("dsyn"))
     (20542 "Hypertension, Pulmonary" ("dsyn"))
     (21311 "Infection" ("dsyn"))
     (22661 "Kidney Failure, Chronic" ("dsyn"))
     (399498 "Oral lichenoid reaction" ("patf"))
     (24901 "Mastocytosis, Diffuse Cutaneous" ("dsyn"))
     (26272 "Mixed Connective Tissue Disease" ("dsyn"))
     (699748 "Pathogenesis" ("patf"))
     (28754 "Obesity" ("dsyn"))
     (7137 "Squamous cell carcinoma" ("neop"))
     (31154 "Peritonitis" ("dsyn"))
     (867389 "Chronic graft-versus-host disease" ("patf"))
     (31763 "Photosensitization" ("dsyn"))
     (7140 "Carcinosarcoma" ("neop"))
     (32285 "Pneumonia" ("dsyn"))
     (867389 "Chronic graft-versus-host disease" ("patf"))
     (33687 "Proteinuria" ("dsyn"))
     (7847 "Malignant neoplasm of cervix uteri" ("neop"))
     (34069 "Pulmonary Fibrosis" ("dsyn"))
     (34155 "Purpura, Thrombotic Thrombocytopenic" ("dsyn"))
     (8479 "Chondrosarcoma" ("neop"))
     (35435 "Rheumatism" ("dsyn"))
     (8487 "Chordoma" ("neop"))
     (36421 "Systemic Scleroderma" ("dsyn"))
     (8487 "Chordoma" ("neop"))
     (10606 "Adenoid Cystic Carcinoma" ("neop"))
     (10606 "Adenoid Cystic Carcinoma" ("neop"))
     (39082 "Syndrome" ("dsyn"))
     (39106 "Pigmented villonodular synovitis" ("dsyn"))
     (40034 "Thrombocytopenia" ("dsyn"))
     (42384 "Vasculitis" ("dsyn"))
     (18206 "granulosa cell tumor" ("neop"))
     (152171 "Primary pulmonary hypertension" ("dsyn"))
     (162835 "Hypopigmentation" ("dsyn"))
     (206061 "Pneumonitis, Interstitial" ("dsyn"))
     (23435 "Leukemia, B-Cell, Acute" ("neop"))
     (267437 "Allergic diarrhea" ("dsyn"))
     (282548 "Leukostasis" ("dsyn"))
     (339143 "Thyroid associated opthalmopathies" ("dsyn"))
     (339510 "Vitelliform dystrophy" ("dsyn"))
     (341697 "Renal impairment" ("dsyn"))
     (745091 "Hypereosinophilia" ("dsyn"))
     (745091 "Hypereosinophilia" ("dsyn"))
     (23470 "Myeloid Leukemia" ("neop"))
     (745283 "INFECTIOUS PROCESS" ("dsyn"))
     (23470 "Myeloid Leukemia" ("neop"))
     (748159 "PULMONARY INVOLVEMENT" ("dsyn"))
     (23472 "Leukemia, Myeloid, Aggressive-Phase" ("neop"))
     (836924 "thrombocytosis" ("dsyn"))
     (23472 "Leukemia, Myeloid, Aggressive-Phase" ("neop"))
     (949690 "Spondylarthritis" ("dsyn"))
     (1112486 "Aggressive Systemic Mastocytosis" ("dsyn"))
     (1136033 "Cutaneous Mastocytosis" ("dsyn"))
     (1142420 "Hepatitis B reactivation" ("dsyn"))
     (1261469 "End stage renal failure" ("dsyn"))
     (23479 "Leukemia, Myelomonocytic, Acute" ("neop"))
     (1279945 "Acute interstitial pneumonia" ("dsyn"))
     (1368107 "Aplastic bone marrow" ("dsyn"))
     (1619734 "Pulmonary arterial hypertension" ("dsyn"))
     (23487 "Acute Promyelocytic Leukemia" ("neop"))
     (23487 "Acute Promyelocytic Leukemia" ("neop"))
     (23494 "Leukemia, T-Cell, Chronic" ("neop"))
     (23827 "liposarcoma" ("neop"))
     (26987 "Myelofibrosis" ("neop"))
     (29925 "Ovarian Carcinoma" ("neop"))
     (29925 "Ovarian Carcinoma" ("neop"))
     (32463 "Polycythemia Vera" ("neop"))
     (32463 "Polycythemia Vera" ("neop"))
     (35412 "Rhabdomyosarcoma" ("neop"))
     (36220 "Kaposi Sarcoma" ("neop"))
     (36631 "Seminoma" ("neop"))
     (39101 "synovial sarcoma" ("neop"))
     (40100 "Thymoma" ("neop"))
     (79218 "Fibromatosis, Aggressive" ("neop"))
     (79218 "Fibromatosis, Aggressive" ("neop"))
     (151779 "[X]Malignant melanoma of skin, unspecified" ("neop"))
     (205851 "Germ cell tumor" ("neop"))
     (205969 "Thymic Carcinoma" ("neop"))
     (205969 "Thymic Carcinoma" ("neop"))
     (206630 "Endometrial Stromal Sarcoma" ("neop"))
     (206693 "Medullary carcinoma" ("neop"))
     (206698 "Cholangiocarcinoma" ("neop"))
     (206728 "Plexiform Neurofibroma" ("neop"))
     (206728 "Plexiform Neurofibroma" ("neop"))
     (276535 "AIDS with Kaposi's sarcoma" ("neop"))
     (278488 "Breast cancer metastatic" ("neop"))
     (278488 "Breast cancer metastatic" ("neop"))
     (278678 "Metastatic renal cell carcinoma" ("neop"))
     (278694 "Disseminated neuroblastoma" ("neop"))
     (278787 "relapsing chronic myelogenous leukemia" ("neop"))
     (278787 "relapsing chronic myelogenous leukemia" ("neop"))
     (278883 "Metastatic melanoma" ("neop"))
     (278883 "Metastatic melanoma" ("neop"))
     (279549
      "Philadelphia chromosome negative chronic myelogenous leukemia"
      ("neop"))
     (280449 "secondary acute myeloid leukemia" ("neop"))
     (334664 "Mast Cell Neoplasm" ("neop"))
     (338113 "Uterine Corpus Sarcoma" ("neop"))
     (341823 "Epithelial tumor of ovary" ("neop"))
     (345967 "Malignant mesothelioma" ("neop"))
     (345967 "Malignant mesothelioma" ("neop"))
     (346421 "Chronic eosinophilic leukemia" ("neop"))
     (346976 "Secondary malignant neoplasm of pancreas" ("neop"))
     (349640 "[M]Subacute myeloid leukemia" ("neop"))
     (431109 "Choroid Plexus Carcinoma" ("neop"))
     (476089 "Endometrial Carcinoma" ("neop"))
     (476089 "Endometrial Carcinoma" ("neop"))
     (521158 "Recurrent tumor" ("neop"))
     (543478 "Residual Tumor" ("neop"))
     (543478 "Residual Tumor" ("neop"))
     (549379 "Recurrent Carcinoma" ("neop"))
     (598798 "Lymphoid neoplasm" ("neop"))
     (598934 "tumor growth" ("neop"))
     (677936 "Refractory Carcinoma" ("neop"))
     (699889 "Female reproductive neoplasm malignant NOS" ("neop"))
     (740267 "Ocular melanomas" ("neop"))
     (740277 "Bile duct carcinoma" ("neop"))
     (743535 "EOSINOPHILIC GRANULOMATOSIS" ("neop"))
     (751690 "Malignant Peripheral Nerve Sheath Tumor" ("neop"))
     (751690 "Malignant Peripheral Nerve Sheath Tumor" ("neop"))
     (812413 "Malignant Pleural Mesothelioma" ("neop"))
     (855013 "Chondrosarcoma recurrent" ("neop"))
     (936223 "Prostate cancer metastatic" ("neop"))
     (1292778 "Chronic myeloproliferative disorder (morphology)" ("neop"))
     (1292778 "Chronic myeloproliferative disorder (morphology)" ("neop"))
     (1327920 "childhood chronic myelogenous leukemia" ("neop"))
     (1333768 "Gastric Gastrointestinal Stromal Tumor" ("neop"))
     (1334026 "High Risk Gastrointestinal Stromal Tumor" ("neop"))
     (1334026 "High Risk Gastrointestinal Stromal Tumor" ("neop"))
     (1334699 "Mesenchymal Cell Neoplasm" ("neop"))
     (1335711 "Recurrent Mature T- and NK-Cell Non-Hodgkin's Lymphoma" ("neop"))
     (1335713 "Recurrent Meningioma" ("neop"))
     (1335729 "Refractory Neoplasm" ("neop"))
     (1336746 "Thymus Carcinoid Tumor" ("neop"))
     (1540912 "Hypereosinophilic syndrome" ("neop"))
     (1540912 "Hypereosinophilic syndrome" ("neop"))
     (235063 "Respiratory Depression" ("patf"))
     (679222 "functional insufficiency" ("patf"))
     (12634 "Disease" ("dsyn"))
     (1815 "Primary Myelofibrosis" ("neop"))
     (12634 "Disease" ("dsyn"))
     (9566 "Complication" ("patf"))
     (24228 "Lymphatic Diseases" ("dsyn"))
     (24899 "mastocytosis" ("dsyn"))
     (20517 "Hypersensitivity" ("patf"))
     (37354 "Smallpox" ("dsyn"))
     (28778 "Obstruction" ("patf"))
     (221013 "Mastocytosis, Systemic" ("dsyn"))
     (1318485 "Liver regeneration disorder" ("dsyn"))
     (242184 "Hypoxia" ("patf"))
     (9402 "Carcinoma of the Large Intestine" ("neop"))
     (456070 "Growth delay" ("patf"))
     (17638 "Glioma" ("neop"))
     (19829 "Hodgkin Disease" ("neop"))
     (23269 "leiomyosarcoma" ("neop"))
     (23269 "leiomyosarcoma" ("neop"))
     (23453 "Leukemia, Lymphocytic, Acute, L2" ("neop"))
     (23476 "Leukemia, Myeloid, Philadelphia-Positive" ("neop"))
     (23480 "Leukemia, Myelomonocytic, Chronic" ("neop"))
     (23481 "Leukemia, Neutrophilic, Chronic" ("neop"))
     (27022 "Myeloproliferative disease" ("neop"))
     (27819 "Neuroblastoma" ("neop"))
     (29463 "osteosarcoma" ("neop"))
     (85136 "Central Nervous System Neoplasms" ("neop"))
     (149925 "Small cell carcinoma of lung" ("neop"))
     (149925 "Small cell carcinoma of lung" ("neop"))
     (152018 "Esophageal carcinoma" ("neop"))
     (178874 "Neoplasm progression" ("neop"))
     (206093 "Neuroectodermal Tumors" ("neop"))
     (235974 "Pancreatic carcinoma" ("neop"))
     (235974 "Pancreatic carcinoma" ("neop"))
     (238461 "Anaplastic thyroid carcinoma" ("neop"))
     (238462 "Medullary carcinoma of thyroid" ("neop"))
     (278726 "Small cell lung cancer extensive stage" ("neop"))
     (376358 "Malignant neoplasm of prostate" ("neop"))
     (376545 "Hematologic Neoplasms" ("neop"))
     (494165 "Secondary malignant neoplasm of liver" ("neop"))
     (494165 "Secondary malignant neoplasm of liver" ("neop"))
     (555198 "Malignant Glioma" ("neop"))
     (677930 "Primary Neoplasm" ("neop"))
     (699791 "Stomach Carcinoma" ("neop"))
     (750952 "Biliary Tract Cancer" ("neop"))
     (751606 "Adult Acute Lymphocytic Leukemia" ("neop"))
     (860582 "Peritoneal metastases" ("neop"))
     (877373 "Advanced cancer" ("neop"))
     (879615 "Stromal Neoplasm" ("neop"))
     (887833 "Carcinoma, Pancreatic Ductal" ("neop"))
     (920028 "Leukaemia recurrent" ("neop"))
     (1266137 "Gastrointestinal stromal sarcoma" ("neop"))
     (1279296 "Chronic leukemia (category)" ("neop"))
     (1370868 "refractory CML" ("neop"))
     (2395 "Alzheimer's Disease" ("dsyn"))
     (8679 "Chronic Disease" ("dsyn"))
     (5699 "Blast Phase" ("neop"))
     (11847 "Diabetes" ("dsyn"))
     (16059 "Fibrosis" ("patf"))
     (11860 "Diabetes Mellitus, Non-Insulin-Dependent" ("dsyn"))
     (6826 "Malignant Neoplasms" ("neop"))
     (37274 "skin disorder" ("dsyn"))
     (21655 "Insulin Resistance" ("patf"))
     (206141 "Idiopathic Hypereosinophilic Syndrome" ("dsyn"))
     (6826 "Malignant Neoplasms" ("neop"))
     (878773 "Overactive Bladder" ("dsyn"))
     (332448 "Infiltration" ("patf"))
     (1167698 "Leukaemic retinopathy" ("dsyn"))
     (7129 "Merkel cell carcinoma" ("neop"))
     (1258104 "Diffuse Scleroderma" ("dsyn"))
     (920563 "insulin sensitivity" ("patf"))
     (7131 "Carcinoma, Non-Small-Cell Lung" ("neop"))
     (7134 "Renal Cell Carcinoma" ("neop"))
     (17185 "Gastrointestinal Neoplasms" ("neop"))
     (17636 "Glioblastoma" ("neop"))
     (23418 "leukemia" ("neop"))
     (23418 "leukemia" ("neop"))
     (23449 "Leukemia, Lymphocytic, Acute" ("neop"))
     (23467 "Leukemia, Myelocytic, Acute" ("neop"))
     (23473 "Myeloid Leukemia, Chronic" ("neop"))
     (23473 "Myeloid Leukemia, Chronic" ("neop"))
     (23474 "Leukemia, Myeloid, Chronic-Phase" ("neop"))
     (24221 "Lymphangioma" ("neop"))
     (25149 "medulloblastoma" ("neop"))
     (25202 "melanoma" ("neop"))
     (26948 "Mycosis Fungoides" ("neop"))
     (27627 "Neoplasm Metastasis" ("neop"))
     (27651 "Neoplasm" ("neop"))
     (27831 "Neurofibromatosis 1" ("neop"))
     (27859 "Acoustic Neuroma" ("neop"))
     (35335 "Retinoblastoma" ("neop"))
     (85669 "Acute leukemia" ("neop"))
     (152276 "Granulocytic Sarcoma" ("neop"))
     (153658 "Malignant neoplasm of endocrine gland" ("neop"))
     (153690 "Secondary malignant neoplasm of bone" ("neop"))
     (220633 "Intraocular melanoma" ("neop"))
     (238198 "Gastrointestinal Stromal Tumors" ("neop"))
     (238198 "Gastrointestinal Stromal Tumors" ("neop"))
     (242596 "Neoplasm, Residual" ("neop"))
     (279543
      "Philadelphia chromosome positive chronic myelogenous leukemia"
      ("neop"))
     (279671 "Cervical Squamous Cell Carcinoma" ("neop"))
     (280100 "Solid tumor" ("neop"))
     (334486 "Sarcoma, Endometrial Stromal, Low-Grade" ("neop"))
     (334569 "Odontogenic myxoma" ("neop"))
     (346429 "Multiple malignancy" ("neop"))
     (392784 "Dermatofibrosarcoma Protuberans" ("neop"))
     (677886 "Epithelial ovarian cancer" ("neop"))
     (856536 "Philadelphia chromosome positive" ("neop"))
     (1261473 "sarcoma" ("neop"))
     (1261473 "sarcoma" ("neop"))
     (1336869 "Unresectable Malignant Neoplasm" ("neop"))
     (1370723 "Stromal sarcoma" ("neop")))))
;;
;; *final-disease-candidates*
;;
;; The disease candidates produced.
;;
;; Of particular interest to us is:
;;
;; (4096 "Asthma" ("dsyn"))
;;
;; The 111 answers include
;;
;; (11849 "Diabetes Mellitus" ("dsyn"))
;;
;; since SemMedDB doesn't "know" that
;; 
#|
'((10054 "Coronary Arteriosclerosis" ("dsyn"))
  (41107 "Trisomy" ("dsyn"))
  (25517 "Metabolic Diseases" ("dsyn"))
  (15695 "Fatty Liver" ("dsyn"))
  (752304 "Hypoxic-Ischemic Encephalopathy" ("dsyn"))
  (17732 "Glucose Intolerance" ("dsyn"))
  (158981 "Neonatal diabetes mellitus" ("dsyn"))
  (6267 "Bronchiectasis" ("dsyn"))
  (11616 "Contact Dermatitis" ("dsyn"))
  (1519680 "Tumor Immunity" ("dsyn"))
  (242231 "Coronary Stenosis" ("dsyn"))
  (729353 "Subfertility" ("dsyn"))
  (9447 "Common Variable Immunodeficiency" ("dsyn"))
  (30920 "Peptic Ulcer" ("dsyn"))
  (87086 "Thrombus" ("dsyn"))
  (1857 "AIDS related complex" ("dsyn"))
  (14038 "Encephalitis" ("dsyn"))
  (35334 "Retinitis Pigmentosa" ("dsyn"))
  (19163 "Hepatitis B" ("dsyn"))
  (38525 "Subarachnoid Hemorrhage" ("dsyn"))
  (221757 "alpha 1-Antitrypsin Deficiency" ("dsyn"))
  (948089 "Acute coronary syndrome" ("dsyn"))
  (231341 "Premature aging syndrome" ("dsyn"))
  (14553 "Absence Epilepsy" ("dsyn"))
  (19151 "Hepatic Encephalopathy" ("dsyn"))
  (20437 "Hypercalcemia" ("dsyn"))
  (178664 "Glomerulosclerosis" ("dsyn"))
  (4623 "Bacterial Infections" ("dsyn"))
  (15397 "Eye diseases" ("dsyn"))
  (21051 "Immunologic Deficiency Syndromes" ("dsyn"))
  (26848 "Myopathy" ("dsyn"))
  (35304 "Retinal Degeneration" ("dsyn"))
  (38220 "Status Epilepticus" ("dsyn"))
  (85084 "Motor Neuron Disease" ("dsyn"))
  (339573 "Primary open angle glaucoma" ("dsyn"))
  (1285162 "Degenerative disorder" ("dsyn"))
  (1536085 "Geographic atrophy" ("dsyn"))
  (20459 "Hyperinsulinism" ("dsyn"))
  (9319 "Colitis" ("dsyn"))
  (14544 "Epilepsy" ("dsyn"))
  (17601 "Glaucoma" ("dsyn"))
  (19158 "Hepatitis" ("dsyn"))
  (20550 "Hyperthyroidism" ("dsyn"))
  (20615 "hypoglycemia" ("dsyn"))
  (24141 "Lupus Erythematosus, Systemic" ("dsyn"))
  (30305 "Pancreatitis" ("dsyn"))
  (33626 "Protein Deficiency" ("dsyn"))
  (38454 "Cerebrovascular accident" ("dsyn"))
  (151747 "Renal tubular disorder" ("dsyn"))
  (270814 "Spastic syndrome" ("dsyn"))
  (400966 "Non-alcoholic fatty liver" ("dsyn"))
  (1175 "Acquired Immunodeficiency Syndrome" ("dsyn"))
  (1824 "Agranulocytosis" ("dsyn"))
  (2736 "Amyotrophic Lateral Sclerosis" ("dsyn"))
  (4135 "Ataxia Telangiectasia" ("dsyn"))
  (7222 "Cardiovascular Diseases" ("dsyn"))
  (7785 "Cerebral Infarction" ("dsyn"))
  (8312 "Primary biliary cirrhosis" ("dsyn"))
  (8370 "Cholestasis" ("dsyn"))
  (11615 "Dermatitis, Atopic" ("dsyn"))
  (11849 "Diabetes Mellitus" ("dsyn"))
  (11884 "Diabetic Retinopathy" ("dsyn"))
  (13595 "Eczema" ("dsyn"))
  (18799 "Heart Diseases" ("dsyn"))
  (19693 "HIV Infections" ("dsyn"))
  (20179 "Huntington Disease" ("dsyn"))
  (21053 "Immune System Diseases" ("dsyn"))
  (21359 "Infertility" ("dsyn"))
  (21364 "Infertility, Male" ("dsyn"))
  (22116 "Ischemia" ("dsyn"))
  (22660 "Kidney Failure, Acute" ("dsyn"))
  (23530 "Leukopenia" ("dsyn"))
  (23895 "Liver diseases" ("dsyn"))
  (24117 "Chronic Obstructive Airway Disease" ("dsyn"))
  (24312 "Lymphopenia" ("dsyn"))
  (27051 "Myocardial Infarction" ("dsyn"))
  (27765 "nervous system disorder" ("dsyn"))
  (29408 "Degenerative polyarthritis" ("dsyn"))
  (29456 "Osteoporosis" ("dsyn"))
  (30567 "Parkinson Disease" ("dsyn"))
  (32914 "Pre-Eclampsia" ("dsyn"))
  (35305 "Retinal Detachment" ("dsyn"))
  (36690 "Septicemia" ("dsyn"))
  (38644 "Sudden infant death syndrome" ("dsyn"))
  (42024 "Urinary Incontinence" ("dsyn"))
  (42341 "Varicocele" ("dsyn"))
  (42721 "Viral hepatitis" ("dsyn"))
  (42769 "Virus Diseases" ("dsyn"))
  (86543 "Cataract" ("anab" "dsyn"))
  (151650 "Renal fibrosis" ("dsyn"))
  (151744 "Myocardial Ischemia" ("dsyn"))
  (158266 "Degenerative disc disease NOS" ("dsyn"))
  (162871 "Aortic Aneurysm, Abdominal" ("dsyn"))
  (206139 "Lichen Planus, Oral" ("dsyn"))
  (238806 "BONE MASS" ("dsyn"))
  (242350 "Erectile dysfunction" ("dsyn"))
  (242383 "Age related macular degeneration" ("dsyn"))
  (242422 "Parkinsonian Disorders" ("dsyn"))
  (268731 "Renal glomerular disease" ("dsyn"))
  (270994 "Steroid-induced myopathy" ("dsyn"))
  (339527 "Leber's amaurosis" ("dsyn"))
  (340970 "Congenital neutropenia" ("dsyn"))
  (343641 "Human papilloma virus infection" ("dsyn"))
  (456909 "Blind Vision" ("dsyn"))
  (524851 "Neurodegenerative Disorders" ("dsyn"))
  (677607 "Hashimoto Disease" ("dsyn"))
  (857357 "Hepatic pathology" ("dsyn"))
  (917798 "Cerebral Ischemia" ("dsyn"))
  (1281300 "Vascular degeneration" ("dsyn"))
  (1456670 "Nerve Diseases" ("dsyn"))
  (4096 "Asthma" ("dsyn")))
|#

;; Combine two of the queries: which of the Diseases or Syndromes or
;; Neoplastic Processes or Pathologic Functions directly treated by
;; imatinib synonyms are *directly* caused by the genes directly
;; inhibited by the imatinib synonyms?
(map
 (lambda (drug)
   (let ((diseases (run* (q)
                     (fresh (disease e-drug/disease p-st-drug/disease p-ob-drug/disease e-drug/disease-rest
                             gene e-drug/gene p-st-drug/gene e-drug/gene-rest
                             e-gene/disease st-gene/disease ot-gene/disease e-gene/disease-rest)
                       (== disease q)
                       (conde
                         [(== "dsyn" p-ob-drug/disease)]
                         [(== "neop" p-ob-drug/disease)]
                         [(== "patf" p-ob-drug/disease)])
                       (== `(,drug ,disease "TREATS" ,p-st-drug/disease ,p-ob-drug/disease . ,e-drug/disease-rest) e-drug/disease)
                       (== `(,drug ,gene "INHIBITS" ,p-st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
                       (== `(,gene ,disease "CAUSES" ,st-gene/disease ,ot-gene/disease . ,e-gene/disease-rest) e-gene/disease)
                       (edgeo e-drug/disease)
                       (edgeo e-drug/gene)
                       (edgeo e-gene/disease)))))
     (list (length (rem-dups diseases)) drug)))
 '((935989 "imatinib" ("phsu" "orch"))
   (939537 "Imatinib mesylate" ("orch" "phsu"))
   (385728 "CGP 57148" ("phsu" "orch"))
   (906802 "STI571" ("phsu" "orch"))
   (935987 "Gleevec" ("orch" "phsu"))))

;; 286 genes are directly inhibited by some synonym for imitinib
(let ((all-genes
       (map
        (lambda (drug)
          (let ((genes (run* (q)
                         (fresh (gene e-drug/gene p-drug/gene e-drug/gene-rest)                
                           (== gene q)                
                           (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
                           (== "INHIBITS" p-drug/gene)
                           (edgeo e-drug/gene)
                           (fresh (cui name concept-type*)
                             (== `(,cui ,name ,concept-type*) gene)
                             (membero "gngm" concept-type*))))))
            (rem-dups genes)))
        '((935989 "imatinib" ("phsu" "orch"))
          (939537 "Imatinib mesylate" ("orch" "phsu"))
          (385728 "CGP 57148" ("phsu" "orch"))
          (906802 "STI571" ("phsu" "orch"))
          (935987 "Gleevec" ("orch" "phsu"))))))
  (length (apply union* all-genes)))
;; =>
;; 286


;; Once we have decided that asthma is a disease of interest, we might
;; want to see the paths between the 47 genes of interest (including
;; KIT and C-KIT) and the asthma.
;;
;; Query takes 2 minutes on a laptop, single-threaded.  We should be
;; able to speed this up.
;;
;; The only specifc gene that is indirectly linked to asthma through
;; mast cell activation (as opposed to signal transduction, which
;; seems less specific) is KIT, through this chain:
;;
;;    (((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
;;      (1155074 "mast cell activation" ("celf"))
;;      "CAUSES"
;;      "gngm"
;;      "celf"
;;      (12373338))
;;     ((1155074 "mast cell activation" ("celf"))
;;      (4096 "Asthma" ("dsyn"))
;;      "AFFECTS"
;;      "celf"
;;      "dsyn"
;;      (18209484 10352758))
;;     ((4096 "Asthma" ("dsyn"))
;;      (1155074 "mast cell activation" ("celf"))
;;      "MANIFESTATION_OF"
;;      "dsyn"
;;      "celf"
;;      (2741114)))
;;
;; The numbers 12373338, 18209484, 10352758, 2741114 can be turned
;; into PubMed URLs.  For example, the URL for 12373338 is:
;;
;; https://www.ncbi.nlm.nih.gov/pubmed/12373338
;;
;; We can then focus on KIT to get the full chain from Gleevec/imatinib
;; and asthma.  Obviously thi can be automated, and composing the queries
;; (or the cached answers from sub-queries) is straight-forward.
(map
 (lambda (gene)
   (let ((disorders
          (run* (q)
            (fresh (e1 e2 e3 celf disorder disorder-type rest1 rest2 rest3)
              (== (list e1 e2 e3) q)
              (fuzzy-concepto "asthma" disorder)
              (conde
                [(== "dsyn" disorder-type)]
                [(== "neop" disorder-type)]
                [(== "patf" disorder-type)])
              (== `(,gene ,celf "CAUSES" "gngm" "celf" . ,rest1) e1)
              (== `(,celf ,disorder "AFFECTS" "celf" ,disorder-type . ,rest2) e2)
              (== `(,disorder ,celf "MANIFESTATION_OF" ,disorder-type "celf" . ,rest3) e3)
              (edgeo e1)
              (edgeo e3)
              (edgeo e2)))))
     (let ((disorders (rem-dups disorders)))
       disorders)))
 '((1428985 "PDGFD gene" ("aapp" "gngm"))
   (919477 "LCK gene" ("aapp" "enzy" "gngm"))
   (1136340 "Semaphorins" ("bacs" "gngm" "aapp"))
   (1366876 "MAPK14 gene" ("gngm" "aapp" "enzy"))
   (1364818 "APP gene" ("enzy" "gngm" "bacs" "aapp" "imft"))
   (1333568 "FLT3 gene" ("gngm" "phsu" "bacs" "aapp"))
   (79050 "c-abl Proto-Oncogenes" ("aapp" "gngm"))
   (79413 "Genes, abl" ("gngm" "aapp"))
   (812253 "CRKL gene" ("bacs" "aapp" "gngm"))
   (915156 "Ephrin Receptor EphA8" ("gngm" "enzy" "aapp"))
   (2716 "Amyloid" ("bacs" "aapp" "gngm"))
   (3241 "Antibodies" ("gngm" "aapp" "imft"))
   (33640 "PROTEIN KINASE" ("gngm" "enzy" "aapp"))
   (33681 "Protein Tyrosine Kinase" ("enzy" "gngm" "aapp"))
   (164786 "Proto-Oncogene Proteins c-akt" ("gngm" "aapp" "enzy"))
   (33684 "Proteins" ("bacs" "gngm" "aapp"))
   (246681 "platelet-derived growth factor BB" ("gngm" "phsu" "aapp"))
   (290068
    "Platelet-Derived Growth Factor beta Receptor"
    ("aapp" "gngm" "rcpt" "enzy"))
   (812228 "AKT1 gene" ("aapp" "phsu" "enzy" "gngm" "bacs"))
   (812375 "ELK3 gene" ("enzy" "gngm" "bacs" "aapp"))
   (1335239 "PPBP gene" ("bacs" "aapp" "gngm"))
   (1419240 "RAD51 gene" ("enzy" "gngm" "aapp"))
   (1421416 "UVRAG gene" ("gngm" "phsu" "aapp"))
   (1422009 "TP63 gene" ("rcpt" "phsu" "imft" "aapp" "gngm"))
   (1424677 "CKAP4 gene" ("gngm" "aapp" "bacs" "phsu"))
   (1425835 "KCNH8 gene" ("gngm" "aapp" "bacs"))
   (1439347 "BTG1 gene" ("gngm" "aapp"))
   (4891 "Fusion Proteins, bcr-abl" ("aapp" "gngm" "bacs"))
   (1439337 "tyrosine kinase ABL1" ("aapp" "gngm" "enzy"))
   (80092
    "Macrophage Colony-Stimulating Factor Receptor"
    ("enzy" "aapp" "imft" "gngm"))
   (879468 "CSF1R gene" ("aapp" "imft" "rcpt" "gngm" "enzy"))
   (32200 "Platelet-Derived Growth Factor" ("gngm" "aapp" "bacs"))
   (72470 "Proto-Oncogene Protein c-kit" ("aapp" "gngm" "rcpt" "imft"))
   (206364 "Receptor Protein-Tyrosine Kinases" ("enzy" "rcpt" "gngm" "aapp"))
   (290067
    "Platelet-Derived Growth Factor alpha Receptor"
    ("rcpt" "aapp" "gngm" "enzy"))
   (174680 "Cyclin D1" ("gngm" "bacs" "aapp"))
   (812385 "BCR gene" ("gngm" "bacs" "enzy" "aapp"))
   (1335202 "PDGFRB gene" ("bacs" "gngm" "rcpt" "enzy" "aapp"))
   (597357 "receptor" ("aapp" "gngm" "rcpt"))
   (31727 "Phosphotransferases" ("aapp" "gngm" "enzy"))
   (1412097 "ABL1 gene" ("imft" "enzy" "gngm" "aapp" "bacs" "phsu"))
   (71253 "Platelet-Derived Growth Factor Receptor" ("aapp" "gngm" "enzy"))
   (1826328 "MTTP gene" ("aapp" "lipd" "gngm" "imft" "phsu" "bacs"))
   (79427 "Tumor Suppressor Genes" ("gngm" "aapp"))
   (105770 "beta catenin" ("aapp" "gngm" "bacs"))
   (920288 "C-KIT Gene" ("gngm" "aapp"))
   (1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))))
;; =>
;; cpu time: 127629 real time: 128505 gc time: 1446
#|
'(()
  ((((919477 "LCK gene" ("aapp" "enzy" "gngm"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (24564241 1281217))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910))))
  ()
  ((((1366876 "MAPK14 gene" ("gngm" "aapp" "enzy"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (26467500))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910))))
  ((((1364818 "APP gene" ("enzy" "gngm" "bacs" "aapp" "imft"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (24188406
      24188406
      22841885
      22841885
      21978835
      21978835
      20597478
      20597478
      18256384
      12176746
      12176746))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910))))
  ((((1333568 "FLT3 gene" ("gngm" "phsu" "bacs" "aapp"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (23340802 23340802))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910))))
  ()
  ()
  ()
  ((((915156 "Ephrin Receptor EphA8" ("gngm" "enzy" "aapp"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (16789903 11409908 7835966))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910))))
  ((((2716 "Amyloid" ("bacs" "aapp" "gngm"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (26758977 25633229))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910))))
  ((((3241 "Antibodies" ("gngm" "aapp" "imft"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (22262845
      19170657
      19048108
      17503113
      15163542
      14620151
      14580993
      12949238
      12482196
      12482196
      12324469
      9450748))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910)))
   (((3241 "Antibodies" ("gngm" "aapp" "imft"))
     (1155074 "mast cell activation" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (25539676 16461989))
    ((1155074 "mast cell activation" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (18209484 10352758))
    ((4096 "Asthma" ("dsyn"))
     (1155074 "mast cell activation" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (2741114))))
  ((((33640 "PROTEIN KINASE" ("gngm" "enzy" "aapp"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (22357971 19290922 18492778 16790031 16790031 16415076))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910))))
  ((((33681 "Protein Tyrosine Kinase" ("enzy" "gngm" "aapp"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (17426060 12035499 7882988))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910))))
  ((((164786 "Proto-Oncogene Proteins c-akt" ("gngm" "aapp" "enzy"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (23222563))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910))))
  ((((33684 "Proteins" ("bacs" "gngm" "aapp"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (26023679
      24850739
      23682925
      23660250
      22498774
      22319212
      21601104
      20036637
      17203870
      15180972
      15180972
      15053611
      12670482
      12477288
      12194978
      12112690
      11175815
      11073315
      9676989
      8616803
      7902881))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910)))
   (((33684 "Proteins" ("bacs" "gngm" "aapp"))
     (1155074 "mast cell activation" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (12393403))
    ((1155074 "mast cell activation" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (18209484 10352758))
    ((4096 "Asthma" ("dsyn"))
     (1155074 "mast cell activation" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (2741114))))
  ((((246681 "platelet-derived growth factor BB" ("gngm" "phsu" "aapp"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (22095643 16883913 16557224 8619925))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910))))
  ((((290068
      "Platelet-Derived Growth Factor beta Receptor"
      ("aapp" "gngm" "rcpt" "enzy"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (15650217 15590688 9916027))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910))))
  ((((812228 "AKT1 gene" ("aapp" "phsu" "enzy" "gngm" "bacs"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (23222563 14505491 12960248))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910))))
  ()
  ()
  ()
  ()
  ()
  ()
  ()
  ()
  ()
  ((((1439337 "tyrosine kinase ABL1" ("aapp" "gngm" "enzy"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (9517496))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910))))
  ((((80092
      "Macrophage Colony-Stimulating Factor Receptor"
      ("enzy" "aapp" "imft" "gngm"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (27224507))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910))))
  ()
  ((((32200 "Platelet-Derived Growth Factor" ("gngm" "aapp" "bacs"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (23103565 19711112 19458196 17956356 7802667 2783138))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910))))
  ()
  ((((206364 "Receptor Protein-Tyrosine Kinases" ("enzy" "rcpt" "gngm" "aapp"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (24743893
      20806817
      17118962
      16727886
      15176971
      13678963
      11916364
      11912280
      11802165
      9627110
      9047384
      7784069
      7537742))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910))))
  ()
  ()
  ((((812385 "BCR gene" ("gngm" "bacs" "enzy" "aapp"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (22885698 15887041 15494014 15494014 11406357 11406357))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910))))
  ()
  ((((597357 "receptor" ("aapp" "gngm" "rcpt"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (27904762
      27479325
      27194789
      26748340
      26283964
      26198787
      26001588
      25954136
      25522385
      25457352
      25250214
      24987288
      24847082
      24404331
      24098092
      23380704
      22310710
      22132325
      22014238
      21824992
      21291419
      21050922
      20505987
      20003820
      19888967
      19465516
      19348466
      19203114
      19001047
      18952824
      18380671
      18367502
      17920519
      17632123
      17485341
      17466390
      17373355
      17355284
      17303405
      17254012
      16956790
      16956790
      16371473
      15827888
      15795223
      15583862
      15356058
      14731813
      14731813
      12919066
      12110144
      11544033
      10917832
      10859220
      10762594
      10629036
      10533704
      10374695
      9842573
      9380811
      9136757
      8875430
      7669492
      7532590
      7512770
      1326354
      1323348))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910))))
  ((((31727 "Phosphotransferases" ("aapp" "gngm" "enzy"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (27994757
      26852687
      25367076
      24931696
      23979726
      23862981
      22002603
      21848862
      21629734
      20159963
      19636563
      17408432
      12881713
      11896598
      11491654
      11280802
      11049052
      1840317
      1838150))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910)))
   (((31727 "Phosphotransferases" ("aapp" "gngm" "enzy"))
     (1155074 "mast cell activation" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (20956018))
    ((1155074 "mast cell activation" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (18209484 10352758))
    ((4096 "Asthma" ("dsyn"))
     (1155074 "mast cell activation" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (2741114))))
  ()
  ()
  ()
  ((((79427 "Tumor Suppressor Genes" ("gngm" "aapp"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (17230190 15735964))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910))))
  ((((105770 "beta catenin" ("aapp" "gngm" "bacs"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (25534229 20926645 19717519 10347231))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910))))
  ()
  ((((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
     (37083 "Signal Transduction" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (18538998))
    ((37083 "Signal Transduction" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (24447081 22447942 19530997 19075672 18699801))
    ((4096 "Asthma" ("dsyn"))
     (37083 "Signal Transduction" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (11197605 10087910)))
   (((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
     (1155074 "mast cell activation" ("celf"))
     "CAUSES"
     "gngm"
     "celf"
     (12373338))
    ((1155074 "mast cell activation" ("celf"))
     (4096 "Asthma" ("dsyn"))
     "AFFECTS"
     "celf"
     "dsyn"
     (18209484 10352758))
    ((4096 "Asthma" ("dsyn"))
     (1155074 "mast cell activation" ("celf"))
     "MANIFESTATION_OF"
     "dsyn"
     "celf"
     (2741114)))))
|#



;; Genes inhibited by Gleevec
;;
;; Of the 52 results, at least a few appear to be classes of genes
;; rather than specific genes:
;;
;; (3241 "Antibodies" ("gngm" "aapp" "imft"))
;; (4891 "Fusion Proteins, bcr-abl" ("aapp" "gngm" "bacs"))
;; (31727 "Phosphotransferases" ("aapp" "gngm" "enzy"))
;; (33640 "PROTEIN KINASE" ("gngm" "enzy" "aapp"))
;; (33684 "Proteins" ("bacs" "gngm" "aapp"))
;; (79050 "c-abl Proto-Oncogenes" ("aapp" "gngm"))
;; (79413 "Genes, abl" ("gngm" "aapp"))
;; (79427 "Tumor Suppressor Genes" ("gngm" "aapp"))
;; (80298 "v-src Oncogenes" ("gngm" "aapp" "enzy" "aapp" "gngm" "bacs"))
;; (597357 "receptor" ("aapp" "gngm" "rcpt"))
;; (1136340 "Semaphorins" ("bacs" "gngm" "aapp"))
;;
;; Of course, is something like (33684 "Proteins" ("bacs" "gngm" "aapp"))
;; even considered a gene class?
;;
;; Oh wow--I had missed the first one of the c-kit names before!
;; So there are actually three synonyms, not two.
;;
;; (72470 "Proto-Oncogene Protein c-kit" ("aapp" "gngm" "rcpt" "imft"))
;; (920288 "C-KIT Gene" ("gngm" "aapp"))
;; (1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
;;
;; So, of the 52 results, ~10 are actually categories rather
;; than specific genes.  Of the remaining ~40 specific genes, 3 of
;; them are synonyms/aliases for KIT.
;; 
(time (rem-dups
       (run* (q)
         (fresh (drug gene e-drug/gene p-drug/gene e-drug/gene-rest)          
           (== gene q)          
           (== '(935987 "Gleevec" ("orch" "phsu")) drug)
           (== `(,drug ,gene ,p-drug/gene . ,e-drug/gene-rest) e-drug/gene)
           (== "INHIBITS" p-drug/gene)
           (edgeo e-drug/gene)
           (fresh (cui name concept-type*)
             (== `(,cui ,name ,concept-type*) gene)
             (membero "gngm" concept-type*))))))
;; =>
#|
'((2716 "Amyloid" ("bacs" "aapp" "gngm"))
  (3241 "Antibodies" ("gngm" "aapp" "imft"))
  (4891 "Fusion Proteins, bcr-abl" ("aapp" "gngm" "bacs"))
  (31727 "Phosphotransferases" ("aapp" "gngm" "enzy"))
  (32200 "Platelet-Derived Growth Factor" ("gngm" "aapp" "bacs"))
  (33640 "PROTEIN KINASE" ("gngm" "enzy" "aapp"))
  (33681 "Protein Tyrosine Kinase" ("enzy" "gngm" "aapp"))
  (33684 "Proteins" ("bacs" "gngm" "aapp"))
  (65344
   "Lymphocyte Specific Protein Tyrosine Kinase p56(lck)"
   ("aapp" "gngm" "enzy"))
  (71253 "Platelet-Derived Growth Factor Receptor" ("aapp" "gngm" "enzy"))
  (72470 "Proto-Oncogene Protein c-kit" ("aapp" "gngm" "rcpt" "imft"))
  (79050 "c-abl Proto-Oncogenes" ("aapp" "gngm"))
  (79413 "Genes, abl" ("gngm" "aapp"))
  (79427 "Tumor Suppressor Genes" ("gngm" "aapp"))
  (80298 "v-src Oncogenes" ("gngm" "aapp" "enzy" "aapp" "gngm" "bacs"))
  (80092
   "Macrophage Colony-Stimulating Factor Receptor"
   ("enzy" "aapp" "imft" "gngm"))
  (105770 "beta catenin" ("aapp" "gngm" "bacs"))
  (138965 "protein-tyrosine kinase c-src" ("gngm" "aapp" "enzy"))
  (164786 "Proto-Oncogene Proteins c-akt" ("gngm" "aapp" "enzy"))
  (174680 "Cyclin D1" ("gngm" "bacs" "aapp"))
  (206364 "Receptor Protein-Tyrosine Kinases" ("enzy" "rcpt" "gngm" "aapp"))
  (246681 "platelet-derived growth factor BB" ("gngm" "phsu" "aapp"))
  (287666 "Rad51 Recombinase" ("gngm" "aapp" "enzy"))
  (290067
   "Platelet-Derived Growth Factor alpha Receptor"
   ("rcpt" "aapp" "gngm" "enzy"))
  (290068
   "Platelet-Derived Growth Factor beta Receptor"
   ("aapp" "gngm" "rcpt" "enzy"))
  (390431 "PDGF receptor tyrosine kinase" ("aapp" "gngm"))
  (597357 "receptor" ("aapp" "gngm" "rcpt"))
  (812228 "AKT1 gene" ("aapp" "phsu" "enzy" "gngm" "bacs"))
  (812253 "CRKL gene" ("bacs" "aapp" "gngm"))
  (812375 "ELK3 gene" ("enzy" "gngm" "bacs" "aapp"))
  (812385 "BCR gene" ("gngm" "bacs" "enzy" "aapp"))
  (915156 "Ephrin Receptor EphA8" ("gngm" "enzy" "aapp"))
  (879468 "CSF1R gene" ("aapp" "imft" "rcpt" "gngm" "enzy"))
  (919477 "LCK gene" ("aapp" "enzy" "gngm"))
  (920288 "C-KIT Gene" ("gngm" "aapp"))
  (1136340 "Semaphorins" ("bacs" "gngm" "aapp"))
  (1333568 "FLT3 gene" ("gngm" "phsu" "bacs" "aapp"))
  (1335202 "PDGFRB gene" ("bacs" "gngm" "rcpt" "enzy" "aapp"))
  (1335239 "PPBP gene" ("bacs" "aapp" "gngm"))
  (1364818 "APP gene" ("enzy" "gngm" "bacs" "aapp" "imft"))
  (1366876 "MAPK14 gene" ("gngm" "aapp" "enzy"))
  (1412097 "ABL1 gene" ("imft" "enzy" "gngm" "aapp" "bacs" "phsu"))
  (1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
  (1419240 "RAD51 gene" ("enzy" "gngm" "aapp"))
  (1421416 "UVRAG gene" ("gngm" "phsu" "aapp"))
  (1424677 "CKAP4 gene" ("gngm" "aapp" "bacs" "phsu"))
  (1422009 "TP63 gene" ("rcpt" "phsu" "imft" "aapp" "gngm"))
  (1425835 "KCNH8 gene" ("gngm" "aapp" "bacs"))
  (1428985 "PDGFD gene" ("aapp" "gngm"))
  (1439337 "tyrosine kinase ABL1" ("aapp" "gngm" "enzy"))
  (1439347 "BTG1 gene" ("gngm" "aapp"))
  (1826328 "MTTP gene" ("aapp" "lipd" "gngm" "imft" "phsu" "bacs")))
|#

;; interesting! A direct *causal* link between KIT/C-KIT and asthma
;; seems unknown.  So we are not just trying to connect a drug and
;; disease, but also KIT and the disease.
;;
;; This does make sense, in that if such a direct connection were
;; known, SemMedDB would probably have a direct link between imatinib
;; and asthma.
(map
 (lambda (gene)
   (let ((disorders
          (run* (q)
            (fresh (e0 celf disorder pred disorder-type rest0)
              (== e0 q)
              (fuzzy-concepto "asthma" disorder)
              (== `(,gene ,disorder ,pred "gngm" ,disorder-type . ,rest0) e0)
              (edgeo e0)))))
     (let ((disorders (rem-dups disorders)))
       disorders)))
 '((1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp"))
   (920288 "C-KIT Gene" ("gngm" "aapp"))
   (72470 "Proto-Oncogene Protein c-kit" ("aapp" "gngm" "rcpt" "imft"))))
;; =>
#|
'(()
  (((920288 "C-KIT Gene" ("gngm" "aapp"))
    (155877 "Extrinsic asthma NOS" ("dsyn"))
    "ASSOCIATED_WITH"
    "gngm"
    "dsyn"
    (25337192 22505052)))
  ())
|#



;; Here is a first attempt at pruning down the number of candidate
;; diseases, removing only those diseases known to be treated by our
;; specific drug of interest (Gleevec) rather than all forms of
;; imatinib.
;;
;; We found not only asthma, but a strong suggestion that Gleevec
;; might treat diabetes (something the query writer didn't know before
;; looking at these answers).  It turns out that the connection
;; between Gleevec and diabetes *is* known, but without using an
;; ontology or other ways to connect concepts, not all
;; diabetes-related entries were automatically removed.  Still, we
;; think this is interesting, since the connection to diabletes that
;; was discovered is *indirect*, and simulates the sort of discovery
;; we would hope for in practice.
;;
;;
;; Original comments follow:
;;
;;
;; remove from the 154 disorders (no neoplasms or pathologic functions)
;; of interest those 53 disorders, neoplasms, or pathologic functions
;; directly treated by Gleevec
;;
;; result is 151 diseases, including the useless "Disease" and "Syndrome" and "Degenerative disorder",
;; and including:
;; * at least 4 entries for epilepsy
;; * approximately 7 entries related to kidney/renal disease
;; * approximately 9 entries related to diabetes
;; etc.
;;
;; Seems like there are a number of groups of diseases within the 151
;; diseases.  For example, diabetes is probably worth a look.
;;
;; And, of course, there is the 1 entry for asthma
;;
;; Wow!
;;
;; https://www.nbcnews.com/health/health-news/cancer-drug-gleevec-might-slow-type-1-diabetes-n771241
;;
;; https://www.medscape.com/viewarticle/882089
;;
;; http://www.ajmc.com/newsroom/scientists-find-accidental-cure-for-type-2-diabetes-imatinib
;;
;; March 29, 2016
;;
;; "The cancer drug imatinib—marketed as Gleevec and known as a game-changer for conditions like chronic myeloid leukemia—may prove the same in type 2 diabetes (T2D), according to a study published recently in Diabetes."
;;
;;
;; The article:
;;
;; http://diabetes.diabetesjournals.org/content/65/4/829
;;
;; Oh, SemMedDB does contain this information!  The list of diseases Gleevec treats includes:
;;
;; (11847 "Diabetes" ("dsyn"))
;; (11860 "Diabetes Mellitus, Non-Insulin-Dependent" ("dsyn"))
;;
;; The problem, of course, is that without doing more reasoning using
;; an ontology or external information, or maybe getting lucky with
;; the ISA predicate, the query has no way of knowing that those 9
;; diabetes-related entries are actually related to the entry
;; (11847 "Diabetes" ("dsyn")).
;;
;; Still, I was pleased that *I* learned something from the query, and
;; that the signal was quite strong after all of that filtering.
;;
;; So, with more resoning, perhaps using an ontology, we should be
;; able to reduce the ~150 answers by removing (or at least ranking
;; lower) answers related to the diseases we know Gleevec treats.
;;
;; Seems that the full story is a bit complicated.  There are papers
;; indicating that imatinib/Gleevec treats diabetes in mouse models.
;; At least one clinical trial on humans didn't seem to show
;; improvement.
(set-subtraction
 '((10054 "Coronary Arteriosclerosis" ("dsyn"))
  (41107 "Trisomy" ("dsyn"))
  (25517 "Metabolic Diseases" ("dsyn"))
  (15695 "Fatty Liver" ("dsyn"))
  (752304 "Hypoxic-Ischemic Encephalopathy" ("dsyn"))
  (17732 "Glucose Intolerance" ("dsyn"))
  (158981 "Neonatal diabetes mellitus" ("dsyn"))
  (6267 "Bronchiectasis" ("dsyn"))
  (11616 "Contact Dermatitis" ("dsyn"))
  (32285 "Pneumonia" ("dsyn"))
  (1519680 "Tumor Immunity" ("dsyn"))
  (242231 "Coronary Stenosis" ("dsyn"))
  (729353 "Subfertility" ("dsyn"))
  (9447 "Common Variable Immunodeficiency" ("dsyn"))
  (33860 "Psoriasis" ("dsyn"))
  (30920 "Peptic Ulcer" ("dsyn"))
  (87086 "Thrombus" ("dsyn"))
  (339510 "Vitelliform dystrophy" ("dsyn"))
  (1857 "AIDS related complex" ("dsyn"))
  (14038 "Encephalitis" ("dsyn"))
  (35334 "Retinitis Pigmentosa" ("dsyn"))
  (19163 "Hepatitis B" ("dsyn"))
  (35435 "Rheumatism" ("dsyn"))
  (38525 "Subarachnoid Hemorrhage" ("dsyn"))
  (221757 "alpha 1-Antitrypsin Deficiency" ("dsyn"))
  (948089 "Acute coronary syndrome" ("dsyn"))
  (231341 "Premature aging syndrome" ("dsyn"))
  (14553 "Absence Epilepsy" ("dsyn"))
  (19151 "Hepatic Encephalopathy" ("dsyn"))
  (20437 "Hypercalcemia" ("dsyn"))
  (24899 "mastocytosis" ("dsyn"))
  (178664 "Glomerulosclerosis" ("dsyn"))
  (4153 "Atherosclerosis" ("dsyn"))
  (4623 "Bacterial Infections" ("dsyn"))
  (15397 "Eye diseases" ("dsyn"))
  (21051 "Immunologic Deficiency Syndromes" ("dsyn"))
  (26848 "Myopathy" ("dsyn"))
  (35304 "Retinal Degeneration" ("dsyn"))
  (35309 "Retinal Diseases" ("dsyn"))
  (38220 "Status Epilepticus" ("dsyn"))
  (85084 "Motor Neuron Disease" ("dsyn"))
  (339573 "Primary open angle glaucoma" ("dsyn"))
  (1285162 "Degenerative disorder" ("dsyn"))
  (1290884 "Inflammatory disorder" ("dsyn"))
  (1536085 "Geographic atrophy" ("dsyn"))
  (18133 "Graft-vs-Host Disease" ("dsyn"))
  (20459 "Hyperinsulinism" ("dsyn"))
  (9319 "Colitis" ("dsyn"))
  (11881 "Diabetic Nephropathy" ("dsyn"))
  (14544 "Epilepsy" ("dsyn"))
  (17601 "Glaucoma" ("dsyn"))
  (19158 "Hepatitis" ("dsyn"))
  (20456 "Hyperglycemia" ("dsyn"))
  (20538 "Hypertensive disease" ("dsyn"))
  (20550 "Hyperthyroidism" ("dsyn"))
  (20615 "hypoglycemia" ("dsyn"))
  (24141 "Lupus Erythematosus, Systemic" ("dsyn"))
  (30305 "Pancreatitis" ("dsyn"))
  (33626 "Protein Deficiency" ("dsyn"))
  (36421 "Systemic Scleroderma" ("dsyn"))
  (38454 "Cerebrovascular accident" ("dsyn"))
  (151747 "Renal tubular disorder" ("dsyn"))
  (239946 "Fibrosis, Liver" ("dsyn"))
  (270814 "Spastic syndrome" ("dsyn"))
  (400966 "Non-alcoholic fatty liver" ("dsyn"))
  (878544 "Cardiomyopathies" ("dsyn"))
  (948008 "Ischemic stroke" ("dsyn"))
  (1175 "Acquired Immunodeficiency Syndrome" ("dsyn"))
  (1824 "Agranulocytosis" ("dsyn"))
  (2395 "Alzheimer's Disease" ("dsyn"))
  (2736 "Amyotrophic Lateral Sclerosis" ("dsyn"))
  (2871 "Anemia" ("dsyn"))
  (3873 "Rheumatoid Arthritis" ("dsyn"))
  (4135 "Ataxia Telangiectasia" ("dsyn"))
  (4364 "Autoimmune Diseases" ("dsyn"))
  (7193 "Cardiomyopathy, Dilated" ("dsyn"))
  (7222 "Cardiovascular Diseases" ("dsyn"))
  (7785 "Cerebral Infarction" ("dsyn"))
  (8312 "Primary biliary cirrhosis" ("dsyn"))
  (8370 "Cholestasis" ("dsyn"))
  (11615 "Dermatitis, Atopic" ("dsyn"))
  (11847 "Diabetes" ("dsyn"))
  (11849 "Diabetes Mellitus" ("dsyn"))
  (11854 "Diabetes Mellitus, Insulin-Dependent" ("dsyn"))
  (11860 "Diabetes Mellitus, Non-Insulin-Dependent" ("dsyn"))
  (11884 "Diabetic Retinopathy" ("dsyn"))
  (13595 "Eczema" ("dsyn"))
  (14175 "Endometriosis, site unspecified" ("dsyn"))
  (17152 "Gastritis" ("dsyn"))
  (17658 "Glomerulonephritis" ("dsyn"))
  (18799 "Heart Diseases" ("dsyn"))
  (18801 "Heart failure" ("dsyn"))
  (19693 "HIV Infections" ("dsyn"))
  (20179 "Huntington Disease" ("dsyn"))
  (20542 "Hypertension, Pulmonary" ("dsyn"))
  (21053 "Immune System Diseases" ("dsyn"))
  (21311 "Infection" ("dsyn"))
  (21359 "Infertility" ("dsyn"))
  (21364 "Infertility, Male" ("dsyn"))
  (21390 "Inflammatory Bowel Diseases" ("dsyn"))
  (22116 "Ischemia" ("dsyn"))
  (22658 "Kidney Diseases" ("dsyn"))
  (22660 "Kidney Failure, Acute" ("dsyn"))
  (23530 "Leukopenia" ("dsyn"))
  (23895 "Liver diseases" ("dsyn"))
  (24117 "Chronic Obstructive Airway Disease" ("dsyn"))
  (24312 "Lymphopenia" ("dsyn"))
  (26769 "Multiple Sclerosis" ("dsyn"))
  (27051 "Myocardial Infarction" ("dsyn"))
  (27765 "nervous system disorder" ("dsyn"))
  (28754 "Obesity" ("dsyn"))
  (29408 "Degenerative polyarthritis" ("dsyn"))
  (29456 "Osteoporosis" ("dsyn"))
  (30567 "Parkinson Disease" ("dsyn"))
  (31763 "Photosensitization" ("dsyn"))
  (32914 "Pre-Eclampsia" ("dsyn"))
  (35305 "Retinal Detachment" ("dsyn"))
  (36690 "Septicemia" ("dsyn"))
  (38644 "Sudden infant death syndrome" ("dsyn"))
  (39082 "Syndrome" ("dsyn"))
  (40034 "Thrombocytopenia" ("dsyn"))
  (41296 "Tuberculosis" ("dsyn"))
  (42024 "Urinary Incontinence" ("dsyn"))
  (42341 "Varicocele" ("dsyn"))
  (42721 "Viral hepatitis" ("dsyn"))
  (42769 "Virus Diseases" ("dsyn"))
  (86543 "Cataract" ("anab" "dsyn"))
  (151650 "Renal fibrosis" ("dsyn"))
  (151744 "Myocardial Ischemia" ("dsyn"))
  (158266 "Degenerative disc disease NOS" ("dsyn"))
  (162557 "Liver Failure, Acute" ("dsyn"))
  (162871 "Aortic Aneurysm, Abdominal" ("dsyn"))
  (206139 "Lichen Planus, Oral" ("dsyn"))
  (238806 "BONE MASS" ("dsyn"))
  (242350 "Erectile dysfunction" ("dsyn"))
  (242383 "Age related macular degeneration" ("dsyn"))
  (242422 "Parkinsonian Disorders" ("dsyn"))
  (268731 "Renal glomerular disease" ("dsyn"))
  (270994 "Steroid-induced myopathy" ("dsyn"))
  (339527 "Leber's amaurosis" ("dsyn"))
  (340970 "Congenital neutropenia" ("dsyn"))
  (343641 "Human papilloma virus infection" ("dsyn"))
  (456909 "Blind Vision" ("dsyn"))
  (524851 "Neurodegenerative Disorders" ("dsyn"))
  (677607 "Hashimoto Disease" ("dsyn"))
  (856169 "Endothelial dysfunction" ("dsyn"))
  (857357 "Hepatic pathology" ("dsyn"))
  (917798 "Cerebral Ischemia" ("dsyn"))
  (1281300 "Vascular degeneration" ("dsyn"))
  (1456670 "Nerve Diseases" ("dsyn"))
  (4096 "Asthma" ("dsyn"))
  (12634 "Disease" ("dsyn"))
  (22661 "Kidney Failure, Chronic" ("dsyn"))
  (23882 "Little's Disease" ("dsyn")))
 '((2395 "Alzheimer's Disease" ("dsyn"))
   (8679 "Chronic Disease" ("dsyn"))
   (5699 "Blast Phase" ("neop"))
   (11847 "Diabetes" ("dsyn"))
   (16059 "Fibrosis" ("patf"))
   (11860 "Diabetes Mellitus, Non-Insulin-Dependent" ("dsyn"))
   (37274 "skin disorder" ("dsyn"))
   (21655 "Insulin Resistance" ("patf"))
   (206141 "Idiopathic Hypereosinophilic Syndrome" ("dsyn"))
   (6826 "Malignant Neoplasms" ("neop"))
   (878773 "Overactive Bladder" ("dsyn"))
   (332448 "Infiltration" ("patf"))
   (1167698 "Leukaemic retinopathy" ("dsyn"))
   (7129 "Merkel cell carcinoma" ("neop"))
   (1258104 "Diffuse Scleroderma" ("dsyn"))
   (920563 "insulin sensitivity" ("patf"))
   (7131 "Carcinoma, Non-Small-Cell Lung" ("neop"))
   (7134 "Renal Cell Carcinoma" ("neop"))
   (17185 "Gastrointestinal Neoplasms" ("neop"))
   (17636 "Glioblastoma" ("neop"))
   (23418 "leukemia" ("neop"))
   (23449 "Leukemia, Lymphocytic, Acute" ("neop"))
   (23467 "Leukemia, Myelocytic, Acute" ("neop"))
   (23473 "Myeloid Leukemia, Chronic" ("neop"))
   (23474 "Leukemia, Myeloid, Chronic-Phase" ("neop"))
   (24221 "Lymphangioma" ("neop"))
   (25149 "medulloblastoma" ("neop"))
   (25202 "melanoma" ("neop"))
   (26948 "Mycosis Fungoides" ("neop"))
   (27627 "Neoplasm Metastasis" ("neop"))
   (27651 "Neoplasm" ("neop"))
   (27831 "Neurofibromatosis 1" ("neop"))
   (27859 "Acoustic Neuroma" ("neop"))
   (35335 "Retinoblastoma" ("neop"))
   (85669 "Acute leukemia" ("neop"))
   (152276 "Granulocytic Sarcoma" ("neop"))
   (153658 "Malignant neoplasm of endocrine gland" ("neop"))
   (153690 "Secondary malignant neoplasm of bone" ("neop"))
   (220633 "Intraocular melanoma" ("neop"))
   (238198 "Gastrointestinal Stromal Tumors" ("neop"))
   (242596 "Neoplasm, Residual" ("neop"))
   (279543
    "Philadelphia chromosome positive chronic myelogenous leukemia"
    ("neop"))
   (279671 "Cervical Squamous Cell Carcinoma" ("neop"))
   (280100 "Solid tumor" ("neop"))
   (334486 "Sarcoma, Endometrial Stromal, Low-Grade" ("neop"))
   (334569 "Odontogenic myxoma" ("neop"))
   (346429 "Multiple malignancy" ("neop"))
   (392784 "Dermatofibrosarcoma Protuberans" ("neop"))
   (677886 "Epithelial ovarian cancer" ("neop"))
   (856536 "Philadelphia chromosome positive" ("neop"))
   (1261473 "sarcoma" ("neop"))
   (1336869 "Unresectable Malignant Neoplasm" ("neop"))
   (1370723 "Stromal sarcoma" ("neop"))))
;; =>
#|
'((10054 "Coronary Arteriosclerosis" ("dsyn"))
  (41107 "Trisomy" ("dsyn"))
  (25517 "Metabolic Diseases" ("dsyn"))
  (15695 "Fatty Liver" ("dsyn"))
  (752304 "Hypoxic-Ischemic Encephalopathy" ("dsyn"))
  (17732 "Glucose Intolerance" ("dsyn"))
  (158981 "Neonatal diabetes mellitus" ("dsyn"))
  (6267 "Bronchiectasis" ("dsyn"))
  (11616 "Contact Dermatitis" ("dsyn"))
  (32285 "Pneumonia" ("dsyn"))
  (1519680 "Tumor Immunity" ("dsyn"))
  (242231 "Coronary Stenosis" ("dsyn"))
  (729353 "Subfertility" ("dsyn"))
  (9447 "Common Variable Immunodeficiency" ("dsyn"))
  (33860 "Psoriasis" ("dsyn"))
  (30920 "Peptic Ulcer" ("dsyn"))
  (87086 "Thrombus" ("dsyn"))
  (339510 "Vitelliform dystrophy" ("dsyn"))
  (1857 "AIDS related complex" ("dsyn"))
  (14038 "Encephalitis" ("dsyn"))
  (35334 "Retinitis Pigmentosa" ("dsyn"))
  (19163 "Hepatitis B" ("dsyn"))
  (35435 "Rheumatism" ("dsyn"))
  (38525 "Subarachnoid Hemorrhage" ("dsyn"))
  (221757 "alpha 1-Antitrypsin Deficiency" ("dsyn"))
  (948089 "Acute coronary syndrome" ("dsyn"))
  (231341 "Premature aging syndrome" ("dsyn"))
  (14553 "Absence Epilepsy" ("dsyn"))
  (19151 "Hepatic Encephalopathy" ("dsyn"))
  (20437 "Hypercalcemia" ("dsyn"))
  (24899 "mastocytosis" ("dsyn"))
  (178664 "Glomerulosclerosis" ("dsyn"))
  (4153 "Atherosclerosis" ("dsyn"))
  (4623 "Bacterial Infections" ("dsyn"))
  (15397 "Eye diseases" ("dsyn"))
  (21051 "Immunologic Deficiency Syndromes" ("dsyn"))
  (26848 "Myopathy" ("dsyn"))
  (35304 "Retinal Degeneration" ("dsyn"))
  (35309 "Retinal Diseases" ("dsyn"))
  (38220 "Status Epilepticus" ("dsyn"))
  (85084 "Motor Neuron Disease" ("dsyn"))
  (339573 "Primary open angle glaucoma" ("dsyn"))
  (1285162 "Degenerative disorder" ("dsyn"))
  (1290884 "Inflammatory disorder" ("dsyn"))
  (1536085 "Geographic atrophy" ("dsyn"))
  (18133 "Graft-vs-Host Disease" ("dsyn"))
  (20459 "Hyperinsulinism" ("dsyn"))
  (9319 "Colitis" ("dsyn"))
  (11881 "Diabetic Nephropathy" ("dsyn"))
  (14544 "Epilepsy" ("dsyn"))
  (17601 "Glaucoma" ("dsyn"))
  (19158 "Hepatitis" ("dsyn"))
  (20456 "Hyperglycemia" ("dsyn"))
  (20538 "Hypertensive disease" ("dsyn"))
  (20550 "Hyperthyroidism" ("dsyn"))
  (20615 "hypoglycemia" ("dsyn"))
  (24141 "Lupus Erythematosus, Systemic" ("dsyn"))
  (30305 "Pancreatitis" ("dsyn"))
  (33626 "Protein Deficiency" ("dsyn"))
  (36421 "Systemic Scleroderma" ("dsyn"))
  (38454 "Cerebrovascular accident" ("dsyn"))
  (151747 "Renal tubular disorder" ("dsyn"))
  (239946 "Fibrosis, Liver" ("dsyn"))
  (270814 "Spastic syndrome" ("dsyn"))
  (400966 "Non-alcoholic fatty liver" ("dsyn"))
  (878544 "Cardiomyopathies" ("dsyn"))
  (948008 "Ischemic stroke" ("dsyn"))
  (1175 "Acquired Immunodeficiency Syndrome" ("dsyn"))
  (1824 "Agranulocytosis" ("dsyn"))
  (2736 "Amyotrophic Lateral Sclerosis" ("dsyn"))
  (2871 "Anemia" ("dsyn"))
  (3873 "Rheumatoid Arthritis" ("dsyn"))
  (4135 "Ataxia Telangiectasia" ("dsyn"))
  (4364 "Autoimmune Diseases" ("dsyn"))
  (7193 "Cardiomyopathy, Dilated" ("dsyn"))
  (7222 "Cardiovascular Diseases" ("dsyn"))
  (7785 "Cerebral Infarction" ("dsyn"))
  (8312 "Primary biliary cirrhosis" ("dsyn"))
  (8370 "Cholestasis" ("dsyn"))
  (11615 "Dermatitis, Atopic" ("dsyn"))
  (11849 "Diabetes Mellitus" ("dsyn"))
  (11854 "Diabetes Mellitus, Insulin-Dependent" ("dsyn"))
  (11884 "Diabetic Retinopathy" ("dsyn"))
  (13595 "Eczema" ("dsyn"))
  (14175 "Endometriosis, site unspecified" ("dsyn"))
  (17152 "Gastritis" ("dsyn"))
  (17658 "Glomerulonephritis" ("dsyn"))
  (18799 "Heart Diseases" ("dsyn"))
  (18801 "Heart failure" ("dsyn"))
  (19693 "HIV Infections" ("dsyn"))
  (20179 "Huntington Disease" ("dsyn"))
  (20542 "Hypertension, Pulmonary" ("dsyn"))
  (21053 "Immune System Diseases" ("dsyn"))
  (21311 "Infection" ("dsyn"))
  (21359 "Infertility" ("dsyn"))
  (21364 "Infertility, Male" ("dsyn"))
  (21390 "Inflammatory Bowel Diseases" ("dsyn"))
  (22116 "Ischemia" ("dsyn"))
  (22658 "Kidney Diseases" ("dsyn"))
  (22660 "Kidney Failure, Acute" ("dsyn"))
  (23530 "Leukopenia" ("dsyn"))
  (23895 "Liver diseases" ("dsyn"))
  (24117 "Chronic Obstructive Airway Disease" ("dsyn"))
  (24312 "Lymphopenia" ("dsyn"))
  (26769 "Multiple Sclerosis" ("dsyn"))
  (27051 "Myocardial Infarction" ("dsyn"))
  (27765 "nervous system disorder" ("dsyn"))
  (28754 "Obesity" ("dsyn"))
  (29408 "Degenerative polyarthritis" ("dsyn"))
  (29456 "Osteoporosis" ("dsyn"))
  (30567 "Parkinson Disease" ("dsyn"))
  (31763 "Photosensitization" ("dsyn"))
  (32914 "Pre-Eclampsia" ("dsyn"))
  (35305 "Retinal Detachment" ("dsyn"))
  (36690 "Septicemia" ("dsyn"))
  (38644 "Sudden infant death syndrome" ("dsyn"))
  (39082 "Syndrome" ("dsyn"))
  (40034 "Thrombocytopenia" ("dsyn"))
  (41296 "Tuberculosis" ("dsyn"))
  (42024 "Urinary Incontinence" ("dsyn"))
  (42341 "Varicocele" ("dsyn"))
  (42721 "Viral hepatitis" ("dsyn"))
  (42769 "Virus Diseases" ("dsyn"))
  (86543 "Cataract" ("anab" "dsyn"))
  (151650 "Renal fibrosis" ("dsyn"))
  (151744 "Myocardial Ischemia" ("dsyn"))
  (158266 "Degenerative disc disease NOS" ("dsyn"))
  (162557 "Liver Failure, Acute" ("dsyn"))
  (162871 "Aortic Aneurysm, Abdominal" ("dsyn"))
  (206139 "Lichen Planus, Oral" ("dsyn"))
  (238806 "BONE MASS" ("dsyn"))
  (242350 "Erectile dysfunction" ("dsyn"))
  (242383 "Age related macular degeneration" ("dsyn"))
  (242422 "Parkinsonian Disorders" ("dsyn"))
  (268731 "Renal glomerular disease" ("dsyn"))
  (270994 "Steroid-induced myopathy" ("dsyn"))
  (339527 "Leber's amaurosis" ("dsyn"))
  (340970 "Congenital neutropenia" ("dsyn"))
  (343641 "Human papilloma virus infection" ("dsyn"))
  (456909 "Blind Vision" ("dsyn"))
  (524851 "Neurodegenerative Disorders" ("dsyn"))
  (677607 "Hashimoto Disease" ("dsyn"))
  (856169 "Endothelial dysfunction" ("dsyn"))
  (857357 "Hepatic pathology" ("dsyn"))
  (917798 "Cerebral Ischemia" ("dsyn"))
  (1281300 "Vascular degeneration" ("dsyn"))
  (1456670 "Nerve Diseases" ("dsyn"))
  (4096 "Asthma" ("dsyn"))
  (12634 "Disease" ("dsyn"))
  (22661 "Kidney Failure, Chronic" ("dsyn"))
  (23882 "Little's Disease" ("dsyn")))
|#

;; Check that the gene (such as KIT) that causes a celf that affects a
;; disorder is also know to directly cause that disorder?  Seems like
;; this is a basic check we should do, to both increase confidence and
;; perhaps reduce answers.
;;
;; Restrict the disease to "dsyn" or "patf", but not "neop" (since in
;; the case of imatinib/Gleevec, we already know it treats cancer).
;;
;; Try across all 47 genes of interest.
;;
;; cpu time: 18586 real time: 18595 gc time: 286
;;
;; 93 answers
;;
;; includes (4096 "Asthma" ("dsyn")) and (11847 "Diabetes" ("dsyn"))
(apply
 union*
 (map
  (lambda (gene)
    (let ((disorders
           (run* (q)
             (fresh (e0 e1 e2 e3 celf disorder disorder-type rest0 rest1 rest2 rest3)
               (== disorder q)
               (conde
                 [(== "dsyn" disorder-type)]
                 ; [(== "neop" disorder-type)]
                 [(== "patf" disorder-type)])
               (== `(,gene ,disorder "CAUSES" "gngm" ,disorder-type . ,rest0) e0)
               (== `(,gene ,celf "CAUSES" "gngm" "celf" . ,rest1) e1)
               (== `(,celf ,disorder "AFFECTS" "celf" ,disorder-type . ,rest2) e2)
               (== `(,disorder ,celf "MANIFESTATION_OF" ,disorder-type "celf" . ,rest3) e3)
               (edgeo e0)
               (edgeo e3)
               (edgeo e1)
               (edgeo e2)))))
      (rem-dups disorders)))
  '((1428985 "PDGFD gene" ("aapp" "gngm"))
    (919477 "LCK gene" ("aapp" "enzy" "gngm"))
    (1136340 "Semaphorins" ("bacs" "gngm" "aapp"))
    (1366876 "MAPK14 gene" ("gngm" "aapp" "enzy"))
    (1364818 "APP gene" ("enzy" "gngm" "bacs" "aapp" "imft"))
    (1333568 "FLT3 gene" ("gngm" "phsu" "bacs" "aapp"))
    (79050 "c-abl Proto-Oncogenes" ("aapp" "gngm"))
    (79413 "Genes, abl" ("gngm" "aapp"))
    (812253 "CRKL gene" ("bacs" "aapp" "gngm"))
    (915156 "Ephrin Receptor EphA8" ("gngm" "enzy" "aapp"))
    (2716 "Amyloid" ("bacs" "aapp" "gngm"))
    (3241 "Antibodies" ("gngm" "aapp" "imft"))
    (33640 "PROTEIN KINASE" ("gngm" "enzy" "aapp"))
    (33681 "Protein Tyrosine Kinase" ("enzy" "gngm" "aapp"))
    (164786 "Proto-Oncogene Proteins c-akt" ("gngm" "aapp" "enzy"))
    (33684 "Proteins" ("bacs" "gngm" "aapp"))
    (246681 "platelet-derived growth factor BB" ("gngm" "phsu" "aapp"))
    (290068
     "Platelet-Derived Growth Factor beta Receptor"
     ("aapp" "gngm" "rcpt" "enzy"))
    (812228 "AKT1 gene" ("aapp" "phsu" "enzy" "gngm" "bacs"))
    (812375 "ELK3 gene" ("enzy" "gngm" "bacs" "aapp"))
    (1335239 "PPBP gene" ("bacs" "aapp" "gngm"))
    (1419240 "RAD51 gene" ("enzy" "gngm" "aapp"))
    (1421416 "UVRAG gene" ("gngm" "phsu" "aapp"))
    (1422009 "TP63 gene" ("rcpt" "phsu" "imft" "aapp" "gngm"))
    (1424677 "CKAP4 gene" ("gngm" "aapp" "bacs" "phsu"))
    (1425835 "KCNH8 gene" ("gngm" "aapp" "bacs"))
    (1439347 "BTG1 gene" ("gngm" "aapp"))
    (4891 "Fusion Proteins, bcr-abl" ("aapp" "gngm" "bacs"))
    (1439337 "tyrosine kinase ABL1" ("aapp" "gngm" "enzy"))
    (80092
     "Macrophage Colony-Stimulating Factor Receptor"
     ("enzy" "aapp" "imft" "gngm"))
    (879468 "CSF1R gene" ("aapp" "imft" "rcpt" "gngm" "enzy"))
    (32200 "Platelet-Derived Growth Factor" ("gngm" "aapp" "bacs"))
    (72470 "Proto-Oncogene Protein c-kit" ("aapp" "gngm" "rcpt" "imft"))
    (206364 "Receptor Protein-Tyrosine Kinases" ("enzy" "rcpt" "gngm" "aapp"))
    (290067
     "Platelet-Derived Growth Factor alpha Receptor"
     ("rcpt" "aapp" "gngm" "enzy"))
    (174680 "Cyclin D1" ("gngm" "bacs" "aapp"))
    (812385 "BCR gene" ("gngm" "bacs" "enzy" "aapp"))
    (1335202 "PDGFRB gene" ("bacs" "gngm" "rcpt" "enzy" "aapp"))
    (597357 "receptor" ("aapp" "gngm" "rcpt"))
    (31727 "Phosphotransferases" ("aapp" "gngm" "enzy"))
    (1412097 "ABL1 gene" ("imft" "enzy" "gngm" "aapp" "bacs" "phsu"))
    (71253 "Platelet-Derived Growth Factor Receptor" ("aapp" "gngm" "enzy"))
    (1826328 "MTTP gene" ("aapp" "lipd" "gngm" "imft" "phsu" "bacs"))
    (79427 "Tumor Suppressor Genes" ("gngm" "aapp"))
    (105770 "beta catenin" ("aapp" "gngm" "bacs"))
    (920288 "C-KIT Gene" ("gngm" "aapp"))
    (1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp")))))
;; =>
#|
'((7193 "Cardiomyopathy, Dilated" ("dsyn"))
  (19158 "Hepatitis" ("dsyn"))
  (30305 "Pancreatitis" ("dsyn"))
  (3873 "Rheumatoid Arthritis" ("dsyn"))
  (4153 "Atherosclerosis" ("dsyn"))
  (24141 "Lupus Erythematosus, Systemic" ("dsyn"))
  (270814 "Spastic syndrome" ("dsyn"))
  (1290884 "Inflammatory disorder" ("dsyn"))
  (1857 "AIDS related complex" ("dsyn"))
  (17639 "Gliosis" ("patf"))
  (7222 "Cardiovascular Diseases" ("dsyn"))
  (21308 "Infarction" ("patf"))
  (14038 "Encephalitis" ("dsyn"))
  (19151 "Hepatic Encephalopathy" ("dsyn"))
  (22116 "Ischemia" ("dsyn"))
  (22658 "Kidney Diseases" ("dsyn"))
  (333467 "Cytopathic effect" ("patf"))
  (26848 "Myopathy" ("dsyn"))
  (333641 "Atrophic" ("patf"))
  (28754 "Obesity" ("dsyn"))
  (162871 "Aortic Aneurysm, Abdominal" ("dsyn"))
  (339510 "Vitelliform dystrophy" ("dsyn"))
  (1395184 "depolarization" ("patf"))
  (856169 "Endothelial dysfunction" ("dsyn"))
  (1521846 "Permissiveness, Biological Function" ("patf"))
  (18801 "Heart failure" ("dsyn"))
  (2878 "Anemia, Hemolytic" ("dsyn"))
  (18188 "Granuloma" ("patf"))
  (36679 "Diastasis" ("patf"))
  (17658 "Glomerulonephritis" ("dsyn"))
  (4368 "Autoimmunity" ("patf"))
  (24312 "Lymphopenia" ("dsyn"))
  (21655 "Insulin Resistance" ("patf"))
  (29456 "Osteoporosis" ("dsyn"))
  (1155266 "Inflammatory Response" ("patf"))
  (15376 "Extravasation" ("patf"))
  (14175 "Endometriosis, site unspecified" ("dsyn"))
  (27055 "Myocardial Reperfusion Injury" ("patf"))
  (878544 "Cardiomyopathies" ("dsyn"))
  (1383860 "Cardiac Hypertrophy" ("patf"))
  (1519697 "Tumorigenicity" ("patf"))
  (18133 "Graft-vs-Host Disease" ("dsyn"))
  (36974 "Shock" ("patf"))
  (21390 "Inflammatory Bowel Diseases" ("dsyn"))
  (231341 "Premature aging syndrome" ("dsyn"))
  (20564 "Hypertrophy" ("patf"))
  (32914 "Pre-Eclampsia" ("dsyn"))
  (332448 "Infiltration" ("patf"))
  (20456 "Hyperglycemia" ("dsyn"))
  (11164 "Degenerative abnormality" ("patf"))
  (13604 "Edema" ("patf"))
  (4096 "Asthma" ("dsyn"))
  (4364 "Autoimmune Diseases" ("dsyn"))
  (29396 "Ossification, Heterotopic" ("patf"))
  (242656 "Disease Progression" ("patf"))
  (21051 "Immunologic Deficiency Syndromes" ("dsyn"))
  (242184 "Hypoxia" ("patf"))
  (21311 "Infection" ("dsyn"))
  (38454 "Cerebrovascular accident" ("dsyn"))
  (41296 "Tuberculosis" ("dsyn"))
  (9319 "Colitis" ("dsyn"))
  (11615 "Dermatitis, Atopic" ("dsyn"))
  (20517 "Hypersensitivity" ("patf"))
  (32285 "Pneumonia" ("dsyn"))
  (87086 "Thrombus" ("dsyn"))
  (2395 "Alzheimer's Disease" ("dsyn"))
  (30567 "Parkinson Disease" ("dsyn"))
  (917798 "Cerebral Ischemia" ("dsyn"))
  (27540 "Necrosis" ("patf"))
  (30660 "Pathologic Processes" ("patf"))
  (524851 "Neurodegenerative Disorders" ("dsyn"))
  (9566 "Complication" ("patf"))
  (27686 "Pathologic Neovascularization" ("patf"))
  (21368 "Inflammation" ("patf"))
  (20538 "Hypertensive disease" ("dsyn"))
  (24117 "Chronic Obstructive Airway Disease" ("dsyn"))
  (27765 "nervous system disorder" ("dsyn"))
  (39082 "Syndrome" ("dsyn"))
  (29408 "Degenerative polyarthritis" ("dsyn"))
  (16059 "Fibrosis" ("patf"))
  (333951 "Growth arrest" ("patf"))
  (28778 "Obstruction" ("patf"))
  (11847 "Diabetes" ("dsyn"))
  (24899 "mastocytosis" ("dsyn"))
  (27051 "Myocardial Infarction" ("dsyn"))
  (879626 "Adverse effects" ("patf"))
  (20507 "Hyperplasia" ("patf"))
  (2871 "Anemia" ("dsyn"))
  (277785 "Functional disorder" ("patf"))
  (699748 "Pathogenesis" ("patf"))
  (1510411 "metaplastic cell transformation" ("patf"))
  (1140999 "Contraction" ("patf"))
  (12634 "Disease" ("dsyn")))
|#

;; Now, remove all the diseases that imatinib variants are known to treat.
;;
;; 52 left, including asthma
(set-subtraction
 '((7193 "Cardiomyopathy, Dilated" ("dsyn"))
  (19158 "Hepatitis" ("dsyn"))
  (30305 "Pancreatitis" ("dsyn"))
  (3873 "Rheumatoid Arthritis" ("dsyn"))
  (4153 "Atherosclerosis" ("dsyn"))
  (24141 "Lupus Erythematosus, Systemic" ("dsyn"))
  (270814 "Spastic syndrome" ("dsyn"))
  (1290884 "Inflammatory disorder" ("dsyn"))
  (1857 "AIDS related complex" ("dsyn"))
  (17639 "Gliosis" ("patf"))
  (7222 "Cardiovascular Diseases" ("dsyn"))
  (21308 "Infarction" ("patf"))
  (14038 "Encephalitis" ("dsyn"))
  (19151 "Hepatic Encephalopathy" ("dsyn"))
  (22116 "Ischemia" ("dsyn"))
  (22658 "Kidney Diseases" ("dsyn"))
  (333467 "Cytopathic effect" ("patf"))
  (26848 "Myopathy" ("dsyn"))
  (333641 "Atrophic" ("patf"))
  (28754 "Obesity" ("dsyn"))
  (162871 "Aortic Aneurysm, Abdominal" ("dsyn"))
  (339510 "Vitelliform dystrophy" ("dsyn"))
  (1395184 "depolarization" ("patf"))
  (856169 "Endothelial dysfunction" ("dsyn"))
  (1521846 "Permissiveness, Biological Function" ("patf"))
  (18801 "Heart failure" ("dsyn"))
  (2878 "Anemia, Hemolytic" ("dsyn"))
  (18188 "Granuloma" ("patf"))
  (36679 "Diastasis" ("patf"))
  (17658 "Glomerulonephritis" ("dsyn"))
  (4368 "Autoimmunity" ("patf"))
  (24312 "Lymphopenia" ("dsyn"))
  (21655 "Insulin Resistance" ("patf"))
  (29456 "Osteoporosis" ("dsyn"))
  (1155266 "Inflammatory Response" ("patf"))
  (15376 "Extravasation" ("patf"))
  (14175 "Endometriosis, site unspecified" ("dsyn"))
  (27055 "Myocardial Reperfusion Injury" ("patf"))
  (878544 "Cardiomyopathies" ("dsyn"))
  (1383860 "Cardiac Hypertrophy" ("patf"))
  (1519697 "Tumorigenicity" ("patf"))
  (18133 "Graft-vs-Host Disease" ("dsyn"))
  (36974 "Shock" ("patf"))
  (21390 "Inflammatory Bowel Diseases" ("dsyn"))
  (231341 "Premature aging syndrome" ("dsyn"))
  (20564 "Hypertrophy" ("patf"))
  (32914 "Pre-Eclampsia" ("dsyn"))
  (332448 "Infiltration" ("patf"))
  (20456 "Hyperglycemia" ("dsyn"))
  (11164 "Degenerative abnormality" ("patf"))
  (13604 "Edema" ("patf"))
  (4096 "Asthma" ("dsyn"))
  (4364 "Autoimmune Diseases" ("dsyn"))
  (29396 "Ossification, Heterotopic" ("patf"))
  (242656 "Disease Progression" ("patf"))
  (21051 "Immunologic Deficiency Syndromes" ("dsyn"))
  (242184 "Hypoxia" ("patf"))
  (21311 "Infection" ("dsyn"))
  (38454 "Cerebrovascular accident" ("dsyn"))
  (41296 "Tuberculosis" ("dsyn"))
  (9319 "Colitis" ("dsyn"))
  (11615 "Dermatitis, Atopic" ("dsyn"))
  (20517 "Hypersensitivity" ("patf"))
  (32285 "Pneumonia" ("dsyn"))
  (87086 "Thrombus" ("dsyn"))
  (2395 "Alzheimer's Disease" ("dsyn"))
  (30567 "Parkinson Disease" ("dsyn"))
  (917798 "Cerebral Ischemia" ("dsyn"))
  (27540 "Necrosis" ("patf"))
  (30660 "Pathologic Processes" ("patf"))
  (524851 "Neurodegenerative Disorders" ("dsyn"))
  (9566 "Complication" ("patf"))
  (27686 "Pathologic Neovascularization" ("patf"))
  (21368 "Inflammation" ("patf"))
  (20538 "Hypertensive disease" ("dsyn"))
  (24117 "Chronic Obstructive Airway Disease" ("dsyn"))
  (27765 "nervous system disorder" ("dsyn"))
  (39082 "Syndrome" ("dsyn"))
  (29408 "Degenerative polyarthritis" ("dsyn"))
  (16059 "Fibrosis" ("patf"))
  (333951 "Growth arrest" ("patf"))
  (28778 "Obstruction" ("patf"))
  (11847 "Diabetes" ("dsyn"))
  (24899 "mastocytosis" ("dsyn"))
  (27051 "Myocardial Infarction" ("dsyn"))
  (879626 "Adverse effects" ("patf"))
  (20507 "Hyperplasia" ("patf"))
  (2871 "Anemia" ("dsyn"))
  (277785 "Functional disorder" ("patf"))
  (699748 "Pathogenesis" ("patf"))
  (1510411 "metaplastic cell transformation" ("patf"))
  (1140999 "Contraction" ("patf"))
  (12634 "Disease" ("dsyn")))
 '((2871 "Anemia" ("dsyn"))
  (2874 "Aplastic Anemia" ("dsyn"))
  (2895 "Sickle Cell Anemia" ("dsyn"))
  (3047 "Animal Diseases" ("dsyn"))
  (15376 "Extravasation" ("patf"))
  (5684 "Malignant neoplasm of urinary bladder" ("neop"))
  (4153 "Atherosclerosis" ("dsyn"))
  (5684 "Malignant neoplasm of urinary bladder" ("neop"))
  (5940 "Bone Diseases" ("dsyn"))
  (18944 "Hematoma" ("patf"))
  (7193 "Cardiomyopathy, Dilated" ("dsyn"))
  (7682 "CNS disorder" ("dsyn"))
  (20507 "Hyperplasia" ("patf"))
  (6118 "Brain Neoplasms" ("neop"))
  (21368 "Inflammation" ("patf"))
  (8728 "Churg-Strauss Syndrome" ("dsyn"))
  (6142 "Malignant neoplasm of breast" ("neop"))
  (10403 "Cryoglobulinemia" ("dsyn"))
  (11644 "Scleroderma" ("dsyn"))
  (6142 "Malignant neoplasm of breast" ("neop"))
  (29435 "Osteolysis" ("patf"))
  (11854 "Diabetes Mellitus, Insulin-Dependent" ("dsyn"))
  (36429 "Sclerosis" ("patf"))
  (11881 "Diabetic Nephropathy" ("dsyn"))
  (36974 "Shock" ("patf"))
  (7095 "Carcinoid Tumor" ("neop"))
  (14175 "Endometriosis, site unspecified" ("dsyn"))
  (86565 "Liver Dysfunction" ("patf"))
  (7095 "Carcinoid Tumor" ("neop"))
  (15230 "Exanthema" ("dsyn"))
  (151654 "Myocardial fibrosis" ("patf"))
  (15230 "Exanthema" ("dsyn"))
  (7097 "Carcinoma" ("neop"))
  (15624 "Fanconi Syndrome" ("dsyn"))
  (151746 "Abnormal renal function" ("patf"))
  (17152 "Gastritis" ("dsyn"))
  (7103 "Malignant neoplasm of endometrium" ("neop"))
  (17658 "Glomerulonephritis" ("dsyn"))
  (151746 "Abnormal renal function" ("patf"))
  (7114 "Malignant neoplasm of skin" ("neop"))
  (18801 "Heart failure" ("dsyn"))
  (231178 "Chronic failure" ("patf"))
  (19196 "Hepatitis C" ("dsyn"))
  (20456 "Hyperglycemia" ("dsyn"))
  (20538 "Hypertensive disease" ("dsyn"))
  (21141 "Inappropriate ADH Syndrome" ("dsyn"))
  (21390 "Inflammatory Bowel Diseases" ("dsyn"))
  (22658 "Kidney Diseases" ("dsyn"))
  (23882 "Little's Disease" ("dsyn"))
  (23890 "Liver Cirrhosis" ("dsyn"))
  (9404 "Colorectal Neoplasms" ("neop"))
  (24115 "Lung diseases" ("dsyn"))
  (333606 "Dystrophy" ("patf"))
  (24440 "Macular Edema, Cystoid" ("dsyn"))
  (443146 "Autoimmune" ("patf"))
  (26769 "Multiple Sclerosis" ("dsyn"))
  (27697 "Nephritis" ("dsyn"))
  (549593 "kidney functional" ("patf"))
  (27947 "Neutropenia" ("dsyn"))
  (16048 "Fibromatosis" ("neop"))
  (33838 "Kimura Disease" ("dsyn"))
  (33860 "Psoriasis" ("dsyn"))
  (34063 "Pulmonary Edema" ("dsyn"))
  (744813 "Hepatic embolisation" ("patf"))
  (35309 "Retinal Diseases" ("dsyn"))
  (879626 "Adverse effects" ("patf"))
  (35920 "Rubella" ("dsyn"))
  (879626 "Adverse effects" ("patf"))
  (18923 "Hemangiosarcoma" ("neop"))
  (36992 "Short Bowel Syndrome" ("dsyn"))
  (1265815 "Multiple ulcers" ("patf"))
  (38013 "Ankylosing spondylitis" ("dsyn"))
  (19204 "Primary carcinoma of the liver cells" ("neop"))
  (1608322 "Leak NOS" ("patf"))
  (39103 "Synovitis" ("dsyn"))
  (19204 "Primary carcinoma of the liver cells" ("neop"))
  (41296 "Tuberculosis" ("dsyn"))
  (85786 "Hamman-Rich syndrome" ("dsyn"))
  (23434 "Chronic Lymphocytic Leukemia" ("neop"))
  (86438 "Hypogammaglobulinemia" ("dsyn"))
  (151859 "Polyserositis" ("dsyn"))
  (23448 "Lymphoblastic Leukemia" ("neop"))
  (158168 "Villonodular synovitis" ("dsyn"))
  (162557 "Liver Failure, Acute" ("dsyn"))
  (162557 "Liver Failure, Acute" ("dsyn"))
  (206062 "Lung Diseases, Interstitial" ("dsyn"))
  (206143 "Loeffler's Endocarditis" ("dsyn"))
  (236178 "Intraabdominal hemorrhage" ("dsyn"))
  (238644 "anemia; profound" ("dsyn"))
  (238790 "destruction; bone" ("dsyn"))
  (239946 "Fibrosis, Liver" ("dsyn"))
  (263664 "Generalized morphea" ("dsyn"))
  (264939 "Systemic vasculitis" ("dsyn"))
  (23475 "Leukemia, Myeloid, Philadelphia-Negative" ("neop"))
  (272203 "Indolent Systemic Mastocytosis" ("dsyn"))
  (276653 "Invasive pulmonary aspergillosis" ("dsyn"))
  (277554 "Primary disease" ("dsyn"))
  (277556 "Recurrent disease" ("dsyn"))
  (334102 "Lymphangiomatosis" ("dsyn"))
  (23484 "Leukemia, Plasmacytic" ("neop"))
  (340548 "Pulmonary capillary hemangiomatosis" ("dsyn"))
  (23601 "Leydig Cell Tumor" ("neop"))
  (341213 "External gastric fistula" ("dsyn"))
  (24301 "Lymphoma, Follicular" ("neop"))
  (341439 "Chronic liver disease NOS" ("dsyn"))
  (24623 "Malignant neoplasm of stomach" ("neop"))
  (442867 "Malignant disease" ("dsyn"))
  (549567 "Pigmentation Disorders" ("dsyn"))
  (678236 "Rare Diseases" ("dsyn"))
  (743496 "END ORGAN DAMAGE" ("dsyn"))
  (25286 "meningioma" ("neop"))
  (25500 "Mesothelioma" ("neop"))
  (854467 "Myelosuppression" ("dsyn"))
  (26764 "Multiple Myeloma" ("neop"))
  (855227 "Purging" ("dsyn"))
  (26986 "Dysmyelopoietic Syndromes" ("neop"))
  (856169 "Endothelial dysfunction" ("dsyn"))
  (878544 "Cardiomyopathies" ("dsyn"))
  (920627 "Orphan Diseases" ("dsyn"))
  (948008 "Ischemic stroke" ("dsyn"))
  (948908 "Nephrotoxic serum nephritis" ("dsyn"))
  (1273070 "Left ventricular diastolic dysfunction" ("dsyn"))
  (1290884 "Inflammatory disorder" ("dsyn"))
  (27832 "Neurofibromatosis 2" ("neop"))
  (1299884 "Eosinophilic myositis" ("dsyn"))
  (1306759 "Eosinophilic disorder" ("dsyn"))
  (1306759 "Eosinophilic disorder" ("dsyn"))
  (1332309 "Anti-Basement Membrane Glomerulonephritis" ("dsyn"))
  (1533022 "Histiocytic proliferation" ("dsyn"))
  (1565489 "Renal Insufficiency" ("dsyn"))
  (36221 "Mast-Cell Sarcoma" ("neop"))
  (41341 "Tuberous Sclerosis" ("neop"))
  (79731 "B-Cell Lymphomas" ("neop"))
  (79772 "T-Cell Lymphoma" ("neop"))
  (153633 "Malignant neoplasm of brain" ("neop"))
  (153633 "Malignant neoplasm of brain" ("neop"))
  (162678 "Neurofibromatoses" ("neop"))
  (205853 "Neoplasms, Epithelial" ("neop"))
  (206647 "Dermatofibrosarcoma" ("neop"))
  (206647 "Dermatofibrosarcoma" ("neop"))
  (206657 "Sarcoma, Alveolar Soft Part" ("neop"))
  (206754 "Neuroendocrine Tumors" ("neop"))
  (206754 "Neuroendocrine Tumors" ("neop"))
  (220650 "Metastatic malignant neoplasm to brain" ("neop"))
  (238463 "Papillary thyroid carcinoma" ("neop"))
  (242379 "Malignant neoplasm of lung" ("neop"))
  (278517 "Non-small cell lung cancer recurrent" ("neop"))
  (278695 "recurrent neuroblastoma" ("neop"))
  (278704 "Malignant Childhood Neoplasm" ("neop"))
  (278727 "Small cell lung cancer recurrent" ("neop"))
  (279068 "childhood solid tumor" ("neop"))
  (279087 "recurrent Kaposi's sarcoma" ("neop"))
  (281361 "Adenocarcinoma pancreas" ("neop"))
  (302592 "Cervix carcinoma" ("neop"))
  (302592 "Cervix carcinoma" ("neop"))
  (334410 "Leydig cell tumor, malignant" ("neop"))
  (334695 "Endometrial Stromal Tumors" ("neop"))
  (349636 "Pre B-cell acute lymphoblastic leukemia" ("neop"))
  (553580 "Ewings sarcoma" ("neop"))
  (677865 "Brain stem glioma" ("neop"))
  (677865 "Brain stem glioma" ("neop"))
  (685938 "Malignant neoplasm of gastrointestinal tract" ("neop"))
  (686619 "Secondary malignant neoplasm of lymph node" ("neop"))
  (854850 "Mycosis fungoides refractory" ("neop"))
  (855054 "Fibrosarcoma metastatic" ("neop"))
  (855211 "Seminoma of testis" ("neop"))
  (948380 "Colorectal cancer metastatic" ("neop"))
  (948380 "Colorectal cancer metastatic" ("neop"))
  (1266042 "Chromophobe Renal Cell Carcinoma" ("neop"))
  (1266101 "Thymic epithelial neoplasm" ("neop"))
  (1266119 "Solitary fibrous tumor" ("neop"))
  (1266120 "Solitary fibrous tumor, malignant" ("neop"))
  (1300127 "Perivascular epithelial cell tumor" ("neop"))
  (1306837 "Papillary Renal Cell Carcinoma" ("neop"))
  (1318543 "Tenosynovial giant cell tumor" ("neop"))
  (1319185 "Chiasmal glioma" ("neop"))
  (1326912 "Tumorigenesis" ("neop"))
  (1328504 "Hormone-refractory prostate cancer" ("neop"))
  (1328504 "Hormone-refractory prostate cancer" ("neop"))
  (1332884 "Central Nervous System Leukemia" ("neop"))
  (1333614 "Fibrosarcomatous Dermatofibrosarcoma Protuberans" ("neop"))
  (1334432 "Low Risk Gastrointestinal Stromal Tumor" ("neop"))
  (1335996 "Small Intestinal Gastrointestinal Stromal Tumor" ("neop"))
  (1378050 "Oncocytic Neoplasm" ("neop"))
  (1411997 "Acute biphenotypic leukemia" ("neop"))
  (1512409 "Hepatocarcinogenesis" ("neop"))
  (1524028 "Intraepithelial Neoplasia of the Mouse Mammary Gland" ("neop"))
  (3864 "Arthritis" ("dsyn"))
  (3873 "Rheumatoid Arthritis" ("dsyn"))
  (1418 "Adenocarcinoma" ("neop"))
  (4364 "Autoimmune Diseases" ("dsyn"))
  (6272 "Bronchiolitis Obliterans" ("dsyn"))
  (9782 "Connective Tissue Diseases" ("dsyn"))
  (10828 "Cytopenia" ("patf"))
  (11603 "Dermatitis" ("dsyn"))
  (11633 "Dermatomyositis" ("dsyn"))
  (242656 "Disease Progression" ("patf"))
  (14457 "Eosinophilia" ("dsyn"))
  (14457 "Eosinophilia" ("dsyn"))
  (242656 "Disease Progression" ("patf"))
  (18133 "Graft-vs-Host Disease" ("dsyn"))
  (7102 "Malignant tumor of colon" ("neop"))
  (19618 "Histiocytosis" ("dsyn"))
  (243083 "associated disease" ("patf"))
  (19621 "Histiocytosis, Langerhans-Cell" ("dsyn"))
  (7115 "Malignant neoplasm of thyroid" ("neop"))
  (19624 "Histiocytosis, Non-Langerhans-Cell" ("dsyn"))
  (277785 "Functional disorder" ("patf"))
  (19625 "Sinus histiocytosis" ("dsyn"))
  (20542 "Hypertension, Pulmonary" ("dsyn"))
  (21311 "Infection" ("dsyn"))
  (22661 "Kidney Failure, Chronic" ("dsyn"))
  (399498 "Oral lichenoid reaction" ("patf"))
  (24901 "Mastocytosis, Diffuse Cutaneous" ("dsyn"))
  (26272 "Mixed Connective Tissue Disease" ("dsyn"))
  (699748 "Pathogenesis" ("patf"))
  (28754 "Obesity" ("dsyn"))
  (7137 "Squamous cell carcinoma" ("neop"))
  (31154 "Peritonitis" ("dsyn"))
  (867389 "Chronic graft-versus-host disease" ("patf"))
  (31763 "Photosensitization" ("dsyn"))
  (7140 "Carcinosarcoma" ("neop"))
  (32285 "Pneumonia" ("dsyn"))
  (867389 "Chronic graft-versus-host disease" ("patf"))
  (33687 "Proteinuria" ("dsyn"))
  (7847 "Malignant neoplasm of cervix uteri" ("neop"))
  (34069 "Pulmonary Fibrosis" ("dsyn"))
  (34155 "Purpura, Thrombotic Thrombocytopenic" ("dsyn"))
  (8479 "Chondrosarcoma" ("neop"))
  (35435 "Rheumatism" ("dsyn"))
  (8487 "Chordoma" ("neop"))
  (36421 "Systemic Scleroderma" ("dsyn"))
  (8487 "Chordoma" ("neop"))
  (10606 "Adenoid Cystic Carcinoma" ("neop"))
  (10606 "Adenoid Cystic Carcinoma" ("neop"))
  (39082 "Syndrome" ("dsyn"))
  (39106 "Pigmented villonodular synovitis" ("dsyn"))
  (40034 "Thrombocytopenia" ("dsyn"))
  (42384 "Vasculitis" ("dsyn"))
  (18206 "granulosa cell tumor" ("neop"))
  (152171 "Primary pulmonary hypertension" ("dsyn"))
  (162835 "Hypopigmentation" ("dsyn"))
  (206061 "Pneumonitis, Interstitial" ("dsyn"))
  (23435 "Leukemia, B-Cell, Acute" ("neop"))
  (267437 "Allergic diarrhea" ("dsyn"))
  (282548 "Leukostasis" ("dsyn"))
  (339143 "Thyroid associated opthalmopathies" ("dsyn"))
  (339510 "Vitelliform dystrophy" ("dsyn"))
  (341697 "Renal impairment" ("dsyn"))
  (745091 "Hypereosinophilia" ("dsyn"))
  (745091 "Hypereosinophilia" ("dsyn"))
  (23470 "Myeloid Leukemia" ("neop"))
  (745283 "INFECTIOUS PROCESS" ("dsyn"))
  (23470 "Myeloid Leukemia" ("neop"))
  (748159 "PULMONARY INVOLVEMENT" ("dsyn"))
  (23472 "Leukemia, Myeloid, Aggressive-Phase" ("neop"))
  (836924 "thrombocytosis" ("dsyn"))
  (23472 "Leukemia, Myeloid, Aggressive-Phase" ("neop"))
  (949690 "Spondylarthritis" ("dsyn"))
  (1112486 "Aggressive Systemic Mastocytosis" ("dsyn"))
  (1136033 "Cutaneous Mastocytosis" ("dsyn"))
  (1142420 "Hepatitis B reactivation" ("dsyn"))
  (1261469 "End stage renal failure" ("dsyn"))
  (23479 "Leukemia, Myelomonocytic, Acute" ("neop"))
  (1279945 "Acute interstitial pneumonia" ("dsyn"))
  (1368107 "Aplastic bone marrow" ("dsyn"))
  (1619734 "Pulmonary arterial hypertension" ("dsyn"))
  (23487 "Acute Promyelocytic Leukemia" ("neop"))
  (23487 "Acute Promyelocytic Leukemia" ("neop"))
  (23494 "Leukemia, T-Cell, Chronic" ("neop"))
  (23827 "liposarcoma" ("neop"))
  (26987 "Myelofibrosis" ("neop"))
  (29925 "Ovarian Carcinoma" ("neop"))
  (29925 "Ovarian Carcinoma" ("neop"))
  (32463 "Polycythemia Vera" ("neop"))
  (32463 "Polycythemia Vera" ("neop"))
  (35412 "Rhabdomyosarcoma" ("neop"))
  (36220 "Kaposi Sarcoma" ("neop"))
  (36631 "Seminoma" ("neop"))
  (39101 "synovial sarcoma" ("neop"))
  (40100 "Thymoma" ("neop"))
  (79218 "Fibromatosis, Aggressive" ("neop"))
  (79218 "Fibromatosis, Aggressive" ("neop"))
  (151779 "[X]Malignant melanoma of skin, unspecified" ("neop"))
  (205851 "Germ cell tumor" ("neop"))
  (205969 "Thymic Carcinoma" ("neop"))
  (205969 "Thymic Carcinoma" ("neop"))
  (206630 "Endometrial Stromal Sarcoma" ("neop"))
  (206693 "Medullary carcinoma" ("neop"))
  (206698 "Cholangiocarcinoma" ("neop"))
  (206728 "Plexiform Neurofibroma" ("neop"))
  (206728 "Plexiform Neurofibroma" ("neop"))
  (276535 "AIDS with Kaposi's sarcoma" ("neop"))
  (278488 "Breast cancer metastatic" ("neop"))
  (278488 "Breast cancer metastatic" ("neop"))
  (278678 "Metastatic renal cell carcinoma" ("neop"))
  (278694 "Disseminated neuroblastoma" ("neop"))
  (278787 "relapsing chronic myelogenous leukemia" ("neop"))
  (278787 "relapsing chronic myelogenous leukemia" ("neop"))
  (278883 "Metastatic melanoma" ("neop"))
  (278883 "Metastatic melanoma" ("neop"))
  (279549
   "Philadelphia chromosome negative chronic myelogenous leukemia"
   ("neop"))
  (280449 "secondary acute myeloid leukemia" ("neop"))
  (334664 "Mast Cell Neoplasm" ("neop"))
  (338113 "Uterine Corpus Sarcoma" ("neop"))
  (341823 "Epithelial tumor of ovary" ("neop"))
  (345967 "Malignant mesothelioma" ("neop"))
  (345967 "Malignant mesothelioma" ("neop"))
  (346421 "Chronic eosinophilic leukemia" ("neop"))
  (346976 "Secondary malignant neoplasm of pancreas" ("neop"))
  (349640 "[M]Subacute myeloid leukemia" ("neop"))
  (431109 "Choroid Plexus Carcinoma" ("neop"))
  (476089 "Endometrial Carcinoma" ("neop"))
  (476089 "Endometrial Carcinoma" ("neop"))
  (521158 "Recurrent tumor" ("neop"))
  (543478 "Residual Tumor" ("neop"))
  (543478 "Residual Tumor" ("neop"))
  (549379 "Recurrent Carcinoma" ("neop"))
  (598798 "Lymphoid neoplasm" ("neop"))
  (598934 "tumor growth" ("neop"))
  (677936 "Refractory Carcinoma" ("neop"))
  (699889 "Female reproductive neoplasm malignant NOS" ("neop"))
  (740267 "Ocular melanomas" ("neop"))
  (740277 "Bile duct carcinoma" ("neop"))
  (743535 "EOSINOPHILIC GRANULOMATOSIS" ("neop"))
  (751690 "Malignant Peripheral Nerve Sheath Tumor" ("neop"))
  (751690 "Malignant Peripheral Nerve Sheath Tumor" ("neop"))
  (812413 "Malignant Pleural Mesothelioma" ("neop"))
  (855013 "Chondrosarcoma recurrent" ("neop"))
  (936223 "Prostate cancer metastatic" ("neop"))
  (1292778 "Chronic myeloproliferative disorder (morphology)" ("neop"))
  (1292778 "Chronic myeloproliferative disorder (morphology)" ("neop"))
  (1327920 "childhood chronic myelogenous leukemia" ("neop"))
  (1333768 "Gastric Gastrointestinal Stromal Tumor" ("neop"))
  (1334026 "High Risk Gastrointestinal Stromal Tumor" ("neop"))
  (1334026 "High Risk Gastrointestinal Stromal Tumor" ("neop"))
  (1334699 "Mesenchymal Cell Neoplasm" ("neop"))
  (1335711 "Recurrent Mature T- and NK-Cell Non-Hodgkin's Lymphoma" ("neop"))
  (1335713 "Recurrent Meningioma" ("neop"))
  (1335729 "Refractory Neoplasm" ("neop"))
  (1336746 "Thymus Carcinoid Tumor" ("neop"))
  (1540912 "Hypereosinophilic syndrome" ("neop"))
  (1540912 "Hypereosinophilic syndrome" ("neop"))
  (235063 "Respiratory Depression" ("patf"))
  (679222 "functional insufficiency" ("patf"))
  (12634 "Disease" ("dsyn"))
  (1815 "Primary Myelofibrosis" ("neop"))
  (12634 "Disease" ("dsyn"))
  (9566 "Complication" ("patf"))
  (24228 "Lymphatic Diseases" ("dsyn"))
  (24899 "mastocytosis" ("dsyn"))
  (20517 "Hypersensitivity" ("patf"))
  (37354 "Smallpox" ("dsyn"))
  (28778 "Obstruction" ("patf"))
  (221013 "Mastocytosis, Systemic" ("dsyn"))
  (1318485 "Liver regeneration disorder" ("dsyn"))
  (242184 "Hypoxia" ("patf"))
  (9402 "Carcinoma of the Large Intestine" ("neop"))
  (456070 "Growth delay" ("patf"))
  (17638 "Glioma" ("neop"))
  (19829 "Hodgkin Disease" ("neop"))
  (23269 "leiomyosarcoma" ("neop"))
  (23269 "leiomyosarcoma" ("neop"))
  (23453 "Leukemia, Lymphocytic, Acute, L2" ("neop"))
  (23476 "Leukemia, Myeloid, Philadelphia-Positive" ("neop"))
  (23480 "Leukemia, Myelomonocytic, Chronic" ("neop"))
  (23481 "Leukemia, Neutrophilic, Chronic" ("neop"))
  (27022 "Myeloproliferative disease" ("neop"))
  (27819 "Neuroblastoma" ("neop"))
  (29463 "osteosarcoma" ("neop"))
  (85136 "Central Nervous System Neoplasms" ("neop"))
  (149925 "Small cell carcinoma of lung" ("neop"))
  (149925 "Small cell carcinoma of lung" ("neop"))
  (152018 "Esophageal carcinoma" ("neop"))
  (178874 "Neoplasm progression" ("neop"))
  (206093 "Neuroectodermal Tumors" ("neop"))
  (235974 "Pancreatic carcinoma" ("neop"))
  (235974 "Pancreatic carcinoma" ("neop"))
  (238461 "Anaplastic thyroid carcinoma" ("neop"))
  (238462 "Medullary carcinoma of thyroid" ("neop"))
  (278726 "Small cell lung cancer extensive stage" ("neop"))
  (376358 "Malignant neoplasm of prostate" ("neop"))
  (376545 "Hematologic Neoplasms" ("neop"))
  (494165 "Secondary malignant neoplasm of liver" ("neop"))
  (494165 "Secondary malignant neoplasm of liver" ("neop"))
  (555198 "Malignant Glioma" ("neop"))
  (677930 "Primary Neoplasm" ("neop"))
  (699791 "Stomach Carcinoma" ("neop"))
  (750952 "Biliary Tract Cancer" ("neop"))
  (751606 "Adult Acute Lymphocytic Leukemia" ("neop"))
  (860582 "Peritoneal metastases" ("neop"))
  (877373 "Advanced cancer" ("neop"))
  (879615 "Stromal Neoplasm" ("neop"))
  (887833 "Carcinoma, Pancreatic Ductal" ("neop"))
  (920028 "Leukaemia recurrent" ("neop"))
  (1266137 "Gastrointestinal stromal sarcoma" ("neop"))
  (1279296 "Chronic leukemia (category)" ("neop"))
  (1370868 "refractory CML" ("neop"))
  (2395 "Alzheimer's Disease" ("dsyn"))
  (8679 "Chronic Disease" ("dsyn"))
  (5699 "Blast Phase" ("neop"))
  (11847 "Diabetes" ("dsyn"))
  (16059 "Fibrosis" ("patf"))
  (11860 "Diabetes Mellitus, Non-Insulin-Dependent" ("dsyn"))
  (6826 "Malignant Neoplasms" ("neop"))
  (37274 "skin disorder" ("dsyn"))
  (21655 "Insulin Resistance" ("patf"))
  (206141 "Idiopathic Hypereosinophilic Syndrome" ("dsyn"))
  (6826 "Malignant Neoplasms" ("neop"))
  (878773 "Overactive Bladder" ("dsyn"))
  (332448 "Infiltration" ("patf"))
  (1167698 "Leukaemic retinopathy" ("dsyn"))
  (7129 "Merkel cell carcinoma" ("neop"))
  (1258104 "Diffuse Scleroderma" ("dsyn"))
  (920563 "insulin sensitivity" ("patf"))
  (7131 "Carcinoma, Non-Small-Cell Lung" ("neop"))
  (7134 "Renal Cell Carcinoma" ("neop"))
  (17185 "Gastrointestinal Neoplasms" ("neop"))
  (17636 "Glioblastoma" ("neop"))
  (23418 "leukemia" ("neop"))
  (23418 "leukemia" ("neop"))
  (23449 "Leukemia, Lymphocytic, Acute" ("neop"))
  (23467 "Leukemia, Myelocytic, Acute" ("neop"))
  (23473 "Myeloid Leukemia, Chronic" ("neop"))
  (23473 "Myeloid Leukemia, Chronic" ("neop"))
  (23474 "Leukemia, Myeloid, Chronic-Phase" ("neop"))
  (24221 "Lymphangioma" ("neop"))
  (25149 "medulloblastoma" ("neop"))
  (25202 "melanoma" ("neop"))
  (26948 "Mycosis Fungoides" ("neop"))
  (27627 "Neoplasm Metastasis" ("neop"))
  (27651 "Neoplasm" ("neop"))
  (27831 "Neurofibromatosis 1" ("neop"))
  (27859 "Acoustic Neuroma" ("neop"))
  (35335 "Retinoblastoma" ("neop"))
  (85669 "Acute leukemia" ("neop"))
  (152276 "Granulocytic Sarcoma" ("neop"))
  (153658 "Malignant neoplasm of endocrine gland" ("neop"))
  (153690 "Secondary malignant neoplasm of bone" ("neop"))
  (220633 "Intraocular melanoma" ("neop"))
  (238198 "Gastrointestinal Stromal Tumors" ("neop"))
  (238198 "Gastrointestinal Stromal Tumors" ("neop"))
  (242596 "Neoplasm, Residual" ("neop"))
  (279543
   "Philadelphia chromosome positive chronic myelogenous leukemia"
   ("neop"))
  (279671 "Cervical Squamous Cell Carcinoma" ("neop"))
  (280100 "Solid tumor" ("neop"))
  (334486 "Sarcoma, Endometrial Stromal, Low-Grade" ("neop"))
  (334569 "Odontogenic myxoma" ("neop"))
  (346429 "Multiple malignancy" ("neop"))
  (392784 "Dermatofibrosarcoma Protuberans" ("neop"))
  (677886 "Epithelial ovarian cancer" ("neop"))
  (856536 "Philadelphia chromosome positive" ("neop"))
  (1261473 "sarcoma" ("neop"))
  (1261473 "sarcoma" ("neop"))
  (1336869 "Unresectable Malignant Neoplasm" ("neop"))
  (1370723 "Stromal sarcoma" ("neop"))))
;; =>
#|
'((19158 "Hepatitis" ("dsyn"))
  (30305 "Pancreatitis" ("dsyn"))
  (24141 "Lupus Erythematosus, Systemic" ("dsyn"))
  (270814 "Spastic syndrome" ("dsyn"))
  (1857 "AIDS related complex" ("dsyn"))
  (17639 "Gliosis" ("patf"))
  (7222 "Cardiovascular Diseases" ("dsyn"))
  (21308 "Infarction" ("patf"))
  (14038 "Encephalitis" ("dsyn"))
  (19151 "Hepatic Encephalopathy" ("dsyn"))
  (22116 "Ischemia" ("dsyn"))
  (333467 "Cytopathic effect" ("patf"))
  (26848 "Myopathy" ("dsyn"))
  (333641 "Atrophic" ("patf"))
  (162871 "Aortic Aneurysm, Abdominal" ("dsyn"))
  (1395184 "depolarization" ("patf"))
  (1521846 "Permissiveness, Biological Function" ("patf"))
  (2878 "Anemia, Hemolytic" ("dsyn"))
  (18188 "Granuloma" ("patf"))
  (36679 "Diastasis" ("patf"))
  (4368 "Autoimmunity" ("patf"))
  (24312 "Lymphopenia" ("dsyn"))
  (29456 "Osteoporosis" ("dsyn"))
  (1155266 "Inflammatory Response" ("patf"))
  (27055 "Myocardial Reperfusion Injury" ("patf"))
  (1383860 "Cardiac Hypertrophy" ("patf"))
  (1519697 "Tumorigenicity" ("patf"))
  (231341 "Premature aging syndrome" ("dsyn"))
  (20564 "Hypertrophy" ("patf"))
  (32914 "Pre-Eclampsia" ("dsyn"))
  (11164 "Degenerative abnormality" ("patf"))
  (13604 "Edema" ("patf"))
  (4096 "Asthma" ("dsyn"))
  (29396 "Ossification, Heterotopic" ("patf"))
  (21051 "Immunologic Deficiency Syndromes" ("dsyn"))
  (38454 "Cerebrovascular accident" ("dsyn"))
  (9319 "Colitis" ("dsyn"))
  (11615 "Dermatitis, Atopic" ("dsyn"))
  (87086 "Thrombus" ("dsyn"))
  (30567 "Parkinson Disease" ("dsyn"))
  (917798 "Cerebral Ischemia" ("dsyn"))
  (27540 "Necrosis" ("patf"))
  (30660 "Pathologic Processes" ("patf"))
  (524851 "Neurodegenerative Disorders" ("dsyn"))
  (27686 "Pathologic Neovascularization" ("patf"))
  (24117 "Chronic Obstructive Airway Disease" ("dsyn"))
  (27765 "nervous system disorder" ("dsyn"))
  (29408 "Degenerative polyarthritis" ("dsyn"))
  (333951 "Growth arrest" ("patf"))
  (27051 "Myocardial Infarction" ("dsyn"))
  (1510411 "metaplastic cell transformation" ("patf"))
  (1140999 "Contraction" ("patf")))
|#

;; playing with ISA
;; ??? ISA imatinib
;;
;; Gleevec makes sense (brand name), and probably Imatinib mesylate.
;; STI571 is okay, probably. http://chemocare.com/chemotherapy/drug-info/STI-571.aspx
;; And CGP 57148.  https://www.biovision.com/imatinib-mesylate-cgp-57148b-sti-571.html
;;
;; The others seem...less good. 'Therapeutic procedure ISA imatinib' seems non-sensical.
;; 'Protein-tyrosine kinase inhibitor ISA imatinib' seems backwards.
;; '((3392 "Antineoplastic Agents" ("phsu"))
;;   (13216 "Pharmacotherapy" ("topp"))
;;   (13227 "Pharmaceutical Preparations" ("phsu"))
;;   (87111 "Therapeutic procedure" ("topp"))
;;   (385728 "CGP 57148" ("phsu" "orch"))
;;   (543467 "Operative Surgical Procedures" ("topp"))
;;   (906802 "STI571" ("phsu" "orch"))
;;   (935987 "Gleevec" ("orch" "phsu"))
;;   (939537 "Imatinib mesylate" ("orch" "phsu"))
;;   (1268567 "Protein-tyrosine kinase inhibitor" ("phsu"))
;;   (1268567 "Protein-tyrosine kinase inhibitor" ("phsu")))
(time
  (run* (q)
      (fresh (what-is-it drug e-what/drug st-what/drug ot-what/drug e-what/drug-rest)        
        (== what-is-it q)
        (== '(935989 "imatinib" ("phsu" "orch")) drug)
        (== `(,what-is-it ,drug "ISA" ,st-what/drug ,ot-what/drug . ,e-what/drug-rest) e-what/drug)
        (edgeo e-what/drug))))

;; playing with ISA
;; imatinib ISA ???
;; '((3392 "Antineoplastic Agents" ("phsu"))
;;   (13216 "Pharmacotherapy" ("topp"))
;;   (13227 "Pharmaceutical Preparations" ("phsu"))
;;   (13227 "Pharmaceutical Preparations" ("phsu"))
;;   (39796 "The science and art of healing" ("topp"))
;;   (87111 "Therapeutic procedure" ("topp"))
;;   (87111 "Therapeutic procedure" ("topp"))
;;   (243076 "antagonists" ("chvf"))
;;   (418981 "Medical therapy" ("topp"))
;;   (543467 "Operative Surgical Procedures" ("topp"))
;;   (679607 "treatment method" ("topp"))
;;   (920425 "Cancer Treatment" ("topp"))
;;   (935451 "neoplasm/cancer chemotherapy" ("topp"))
;;   (939537 "Imatinib mesylate" ("orch" "phsu"))
;;   (1254351 "Pharmacologic Substance" ("phsu"))
;;   (1268567 "Protein-tyrosine kinase inhibitor" ("phsu"))
;;   (1372955 "Active Ingredients" ("phsu"))
;;   (1449702 "Protein Kinase Inhibitors" ("phsu"))
;;   (1519313 "Signal Transduction Inhibitor" ("phsu"))
;;   (1533685 "Injection procedure" ("topp"))
;;   (1579409 "Molecular Target Inhibitors" ("phsu"))
;;   (1611640 "Therapeutic agent (substance)" ("phsu")))
(time
  (run* (q)
      (fresh (drug what-is-it e-drug/what st-drug/what ot-drug/what e-drug/what-rest)        
        (== what-is-it q)
        (== '(935989 "imatinib" ("phsu" "orch")) drug)
        (== `(,drug ,what-is-it "ISA" ,st-drug/what ,ot-drug/what . ,e-drug/what-rest) e-drug/what)
        (edgeo e-drug/what))))




;; let's test the individual parts of the query

;; compare
;;
;; (32 (920288 "C-KIT Gene" ("gngm" "aapp")))
;;
;; and
;;
;; (35 (1416655 "KIT gene" ("bacs" "imft" "gngm" "aapp")))
;;
;; which cause 32 and 35 diseases, respectively, with these monstrosities:
;;
;; (291 (597357 "receptor" ("aapp" "gngm" "rcpt")))
;; (296 (1823619 "VEGFA gene" ("bacs" "phsu" "rcpt" "gngm" "imft" "enzy" "aapp")))
;; (418 (1456820 "Tumor Necrosis Factor-alpha" ("imft" "gngm" "aapp")))
;; (506 (33684 "Proteins" ("bacs" "gngm" "aapp")))
;; (579 (79189 "cytokine" ("aapp" "imft" "gngm")))
;; (1171 (17337 "Genes" ("aapp" "gngm"))))
;;
;; Could either just drop entries like (1171 (17337 "Genes" ("aapp" "gngm")))
;; and (506 (33684 "Proteins" ("bacs" "gngm" "aapp"))), or prioritize search
;; to start with smallest number of diseases (or both).
;;
;; Also, should make sure to remove duplicate diseases in the results!
;; And remove ridiculous entries like (12634 "Disease" ("dsyn"))
;;
;; Also, should take the union of diseases caused by C-KIT and KIT,
;; rather than trying them both separately (and getting duplicate
;; answers).
;;
;; Should also consider taking the union of all diseases produced by
;; all genes, to avoid duplicate work, or do caching/memoization/tabling.
;;
(let ((genes-inhibited-by-imatinib
       (run* (q)
         (fresh (drug gene known-disease something unknown-disease
                      e-drug/gene st-drug/gene e-drug/gene-rest
                      e-gene/known-disease st-gene/known-disease ot-gene/known-disease e-gene/known-disease-rest
                      e-drug/known-disease e-drug/known-disease-rest
                      e-gene/something p-gene/something st-gene/something ot-gene/something e-gene/something-rest
                      e-something/unknown-disease p-something/unknown-disease st-something/unknown-disease ot-something/unknown-disease e-something/unknown-disease-rest)
          
           (== gene q)

           ;; imatinib inhibits some gene
           (== '(935989 "imatinib" ("phsu" "orch")) drug)
           (== `(,drug ,gene "INHIBITS" ,st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
           (edgeo e-drug/gene)

           ))))
  (let ((genes-inhibited-by-imatinib (rem-dups genes-inhibited-by-imatinib)))
    (sort
      (map (lambda (gene)
             (let ((num-diseases-caused-by-gene
                    (length
                     (run* (q)
                       (fresh (drug known-disease something unknown-disease
                                    e-drug/gene st-drug/gene e-drug/gene-rest
                                    e-gene/known-disease st-gene/known-disease ot-gene/known-disease e-gene/known-disease-rest
                                    e-drug/known-disease e-drug/known-disease-rest
                                    e-gene/something p-gene/something st-gene/something ot-gene/something e-gene/something-rest
                                    e-something/unknown-disease p-something/unknown-disease st-something/unknown-disease ot-something/unknown-disease e-something/unknown-disease-rest)
                       
                         (== e-gene/known-disease q)

                         ;; that gene directly causes some disease...
                         (== `(,gene ,known-disease "CAUSES" ,st-gene/known-disease ,ot-gene/known-disease . ,e-gene/known-disease-rest) e-gene/known-disease)
                         (conde
                           [(== "dsyn" ot-gene/known-disease)]
                           [(== "neop" ot-gene/known-disease)])
                         (edgeo e-gene/known-disease))))))
               (list num-diseases-caused-by-gene gene)))
           genes-inhibited-by-imatinib)
      (lambda (l1 l2) (< (car l1) (car l2))))))

;; how many genes are inhibited by imatinib?
;;
;; cpu time: 10 real time: 10 gc time: 0
;; 213 genes inhibited by imatinib
;;
;; some of these genes are dups!  why?
;; 206 unique genes
;;
;; one of these answers is (17337 "Genes" ("aapp" "gngm")), as opposed to a specific gene--does this result in a degenerate blowup?  This seems fishy!!
(time
  (length
    (run* (q)
      (fresh (drug gene known-disease something unknown-disease
                   e-drug/gene st-drug/gene e-drug/gene-rest
                   e-gene/known-disease st-gene/known-disease ot-gene/known-disease e-gene/known-disease-rest
                   e-drug/known-disease e-drug/known-disease-rest
                   e-gene/something p-gene/something st-gene/something ot-gene/something e-gene/something-rest
                   e-something/unknown-disease p-something/unknown-disease st-something/unknown-disease ot-something/unknown-disease e-something/unknown-disease-rest)
          
        (== gene q)

        ;; imatinib inhibits some gene
        (== '(935989 "imatinib" ("phsu" "orch")) drug)
        (== `(,drug ,gene "INHIBITS" ,st-drug/gene "gngm" . ,e-drug/gene-rest) e-drug/gene)
        (edgeo e-drug/gene)

        ))))
