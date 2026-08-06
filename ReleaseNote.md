# Proline-Server Release Note

## Version 2.4.0 (Snapshot)

* Import DiaNN feature
  * Add PM-diann-parser module 
  * Add "proline/dps/msqImportDiaNNResults" service in Cortex
  * Add parameter parse from log.txt for PTMs and Enzyme (light version) and miss cleavage
  * Fixes experimental design error (quantification dataset number, sample name etc. )
  * Add filtering in Service (None, MBR like, no MBR like)
  * Fixes some properties values (score, elution time...)
  * Add version diaNN 2.5 support 
  * Save diann command line as quant config using new schema name "quantitation.diann_config"
  * Save all qValues in peptideMatch properties
  * [Dev] update dependencies (mzdb, proline-core...) + replace Scala version suffix with classifier

## Version 2.3.x

* Fixes #25785: Export-'Import&Filter': Duplicate Dataset in table when merge exported.
* Update dependencies (proline-admin, proline-core)


## Version 2.3.0

see https://www.profiproteomics.fr/proline/proline-support/ 