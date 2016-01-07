-- Remove all Parsing_Rule entries.
-- New use of this table : 1-1 with SeqDB Instance and updated each time properties file is updated (and SEDB Instance is used for bioSeq Retrieve
  
ALTER TABLE se_db
  DROP COLUMN parsing_rule_id;

DELETE FROM parsing_rule;

DROP INDEX parsing_rule_name_idx;


-- Column release_identifier contains regex to get release version in fasta name 
ALTER TABLE parsing_rule
  RENAME COLUMN release TO release_identifier;
  
ALTER TABLE se_db_instance
  ADD COLUMN parsing_rule_id bigint;

ALTER TABLE se_db_instance
  ADD CONSTRAINT parsing_rule_se_db_instance_fk FOREIGN KEY (parsing_rule_id)
      REFERENCES parsing_rule (id) MATCH SIMPLE
      ON UPDATE NO ACTION ON DELETE SET NULL;

 

 