
---

## Note

:warning:

We are currently migrating all our source code from out actual SVN to Git.
In the meantime, we provide a tar.gz containing the source files of all repository's projects.

---

# Proline-Server

This repository hosts the different JMS Server projects:
  * Proline-Node: A base project to assist the creation of JMS consumers
  * Proline-Cortex-API: services, methods and parameters definition
  * Proline-Cortex: JMS Server implementation

and all other computational modules other than Proline-Core
  * PM-DatasetExporter: Proline Module dedicated to the export of Proline datasets
  * PM-FragmentMatchGenerator: Proline Module made to generate the fragment matches from a spectrum
  * Parsing modules: parse a search engine result file and store its result in databases
    * PM-MascotParser
    * PM-OmssaParser
    * PM-XtandemParser
    * PM-MaxQuantParser: MaxQuant result parse, for identification and quantitation result
    * PM-MzIdentML :  Parse MzIdentML result and also generate mzIdentML file from Proline dataset
  * PM-MSDiag: Module designed to run a diagnosis of a MS/MS search
  * PM-SequenceRepository: Module to get Protein sequences and associated information from fasta files 
  * UnimodUnmarshaller : Util project to read Unimod description

# License

This project is licensed under the CeCILL License - see the Licence_CeCILL_V2.1-en.txt file for details

# Proline Web Site

 Visit http://www.profiproteomics.fr/proline for Proline Suite documentation and downloads.

