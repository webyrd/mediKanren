use strict;

# Remedy to a data corruption problem in one upstream file.  Repro:
# 
# input file:
#         3f8f7c80dc9c39dc291468fc1ab6ae9b80b5630c  sri-reference-kg-0.3.0.tar.gz
# 
# Run this in a terminal:
# 
#     cat data/sri-reference/0.3.0/sri-reference-kg-0.3.0_nodes.tsv | perl -e 'binmode(STDOUT); while($_ = <>) {$a=$_; chomp($_); $a =~ s/\t/\|/g; @F=split("|",$a); $s=scalar(@F); $i+=1; print "$i: "; $x=$a; $x =~ s/\t/\\t/g; print "$i\t$x";}' | tail -n+849 | head -5 
#     849: 849	FlyBase:FBgn0004623|Gbeta76C|biolink:NamedThing|biolink:GenomicEntity|biolink:Gene|G protein beta-subunit 76C||ncbigene|panther|flybase|go|CG8770|DGqbeta|eGbeta|eye Gbeta|Galpha76C|gbe|Gbe|G[[beta]]|Gbeta|Gbeta76c|G[[beta76C]]|G[[beta]]e|Gbeta[[e]]|Gbeta{e}|Gbetae|G[[beta]] eye|G[q]|Gqbeta|Gqbetae|guanine nucleotide regulatory protein beta subunit||True||||||||||NCBITaxon:7227|NCBITaxon:7227||||||||||SO:0000704|owl:Class|SO:0000704|
#     850: 850	FlyBase:FBgn0004629|Cys|biolink:NamedThing|biolink:GenomicEntity|biolink:Gene|Cystatin-like||ncbigene|flybase|go|anon-EST:Liang-2.34|BcDNA:RH72992|CG8050|clone 2.34|cys|cystatin|Cystatin||True||||||||||NCBITaxon:7227||||||||||owl:Class|SO:0000704|
#     ho|Dmrho|DMRHO|DmRho1|DMRHOa|DMRHOb|DRORHO|iks|Rho|RHO|rho-1|rho1|Rho-1|Rho1|RHOb|rhom|Rhomboid|RHOMBOID|rhomboid-1|rhomboid1|Rhomboid 1|Rhomboid-1|Rhomboid-I|rhomboid/veinlet|ve|Ve|veinlet|Veinlet||True||||||||||NCBITaxon:7227|NCBITaxon:7227||||||||||SO:0000704|owl:Class|SO:0000704|
#     852: 852	FlyBase:FBgn0004636|Rap1|biolink:NamedThing|biolink:GenomicEntity|biolink:Gene|Rap1 GTPase||ncbigene|panther|flybase|go|BEST:GH18528|CG1956|C-ras3|DRap|D-Rap1|DRap1|Dras3|Dras62B|EC3-7|E(faf)|Enhancer of faf|FCG-C|l(3)62Bf|l(3)R|P08645|R|Rap|rap1|RAP1|ras 3|ras3|Ras3|RasIII|roughened|Roughened||True||||||||||NCBITaxon:7227|NCBITaxon:7227||||||||||SO:0000704|owl:Class|SO:0000704|
#     853: 853	FlyBase:FBgn0004638|drk|biolink:NamedThing|biolink:GenomicEntity|biolink:Gene|downstream of receptor kinase||ncbigene|panther|flybase|go|24/1|CG6033|crkl|Downstream of receptor kinase|downstream of receptor kinases|downstream receptor kinase|Drk|DRK|Drk/Grb2|Enhancer of sevenless-2B|E(sev)2B|Grb-2|Grb2|GRB2|Grb2/drk|Grb/drk|l(2)10626|l(2)k13809|P1112|Su(sev)R1||True||||||||||NCBITaxon:7227|NCBITaxon:7227||||||||||SO:0000704|owl:Class|SO:0000704|
# 
# The numbering appears corrupted due to a spurious CR character on line 851.
#
# Note that there really is a line 851 there, but if the output reaches the screen
# as shown above, most terminals will overwrite it.
# 
# Pipe the file through this script to remedy: 
#
# cat data/sri-reference/0.3.0/sri-reference-kg-0.3.0_nodes.tsv \
#   | perl util/data-import/remove_cr.pl \
#   > data/sri-reference/0.3.0/sri-reference-kg-0.3.0_nodes_nocr.tsv 

binmode(STDOUT);
while($_ = <>) {
    $a=$_; 
    $a =~ s/\x0d//g; 
    print "$a";
}
