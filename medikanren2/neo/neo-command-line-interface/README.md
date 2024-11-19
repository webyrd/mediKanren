# mediKanren Command-Line Interface

This command-line interface (CLI) allows users to query the mediKanren biomedical reasoning system to explore relationships between medical concepts, such as identifying drug targets that activate or inhibit specific genes.

## Prerequisites

- [Racket](https://racket-lang.org/) programming language installed on your system.
- Access to the mediKanren knowledge graph database.

## Installation

1. **Clone the Repository**:
   ```bash
   git clone https://github.com/webyrd/mediKanren.git 
   ```
2. **Navigate to the Directory**:
   ```bash
   cd mediKanren/medikanren2/neo/neo-command-line-interface
   ```
3. **Run the Script**:
   ```bash
   racket command-line-interface.rkt
   ```

## Usage

Upon running the script, you'll be prompted to enter commands. The available commands are:

- `activates CURIE ... > output-file`: Identifies entities that activate the specified CURIE(s).
- `inhibits CURIE ... > output-file`: Identifies entities that inhibit the specified CURIE(s).
- `synonyms CURIE ...`: Retrieves synonyms for the specified CURIE(s).
- `all relations CURIE ... > output-file`: Retrieves all relations for the specified CURIE(s).
- `with predicates -predicate PREDICATE ... -curie CURIE ... > output-file`: Retrieves relations for the specified CURIE(s) with given predicates.

## Example Query

To identify drug targets that activate or inhibit a gene of interest:

1. **Identify the CURIE for the Gene**: Determine the CURIE for the gene. For example, the CURIE for the gene *TP53* might be `HGNC:11998`.

2. **Run the `synonyms` Command**:
   ```bash
   synonyms HGNC:11998
   ```
   **Expected Output**:
   ```
   The synonyms of (HGNC:11998) are (HGNC:11998 NCBIGene:7157 UMLS:C0079419 OMIM:191170 REACT:R-HSA-6797244 ENSEMBL:LRG_321 ENSEMBL:ENSG00000141510)
   ```
   You may find the details about the returned entities in the file `synonyms-details.tsv` under the same directory.

3. **Run the `activates` Command**:
   ```bash
   activates HGNC:11998 NCBIGene:7157 UMLS:C0079419 OMIM:191170 REACT:R-HSA-6797244 ENSEMBL:LRG_321 ENSEMBL:ENSG00000141510 > activates_TP53.tsv
   ```
   This command will generate a file named `activates_TP53.tsv` containing entities that activate *TP53* with information about the enities and the relationships.

   *Please note that `HGNC:11998` and all its synonyms retrieved via the `synonyms` command are included in the command.* 

5. **Run the `inhibits` Command**:
   ```bash
   inhibits HGNC:11998 NCBIGene:7157 UMLS:C0079419 OMIM:191170 REACT:R-HSA-6797244 ENSEMBL:LRG_321 ENSEMBL:ENSG00000141510 > inhibits_TP53.tsv
   ```
   This command will generate a file named `inhibits_TP53.tsv` containing entities that inhibit *TP53*.
