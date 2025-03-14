
CREATE TABLE public.parsing_rule (
                id IDENTITY NOT NULL,
                name VARCHAR(255) NOT NULL,
                release VARCHAR(255),
                se_db_identifier VARCHAR(255) NOT NULL,
                repository_identifier VARCHAR(255),
                repo_id_from_se_id VARCHAR(255),
                CONSTRAINT parsing_rule_pk PRIMARY KEY (id)
);
COMMENT ON COLUMN public.parsing_rule.name IS 'UNIQUE .';
COMMENT ON COLUMN public.parsing_rule.release IS 'Rule to parse SEDbInstance version string from source fileName.';


CREATE UNIQUE INDEX public.parsing_rule_name_idx
 ON public.parsing_rule
 ( name );

CREATE TABLE public.repository (
                id IDENTITY NOT NULL,
                name VARCHAR(255) NOT NULL,
                url LONGVARCHAR,
                CONSTRAINT repository_pk PRIMARY KEY (id)
);
COMMENT ON TABLE public.repository IS 'Standard Repository.';
COMMENT ON COLUMN public.repository.name IS 'UNIQUE .';


CREATE UNIQUE INDEX public.repository_name_idx
 ON public.repository
 ( name );

CREATE TABLE public.repository_identifier (
                id IDENTITY NOT NULL,
                value VARCHAR(255) NOT NULL,
                repository_id BIGINT NOT NULL,
                CONSTRAINT repository_identifier_pk PRIMARY KEY (id)
);
COMMENT ON COLUMN public.repository_identifier.value IS 'UNIQUE for given repository.';


CREATE UNIQUE INDEX public.repository_identifier_idx
 ON public.repository_identifier
 ( value, repository_id );

CREATE TABLE public.se_db (
                id IDENTITY NOT NULL,
                name VARCHAR(255) NOT NULL,
                alphabet VARCHAR(3) NOT NULL,
                parsing_rule_id BIGINT,
                repository_id BIGINT,
                CONSTRAINT se_db_pk PRIMARY KEY (id)
);
COMMENT ON TABLE public.se_db IS 'Search Engine Db.';
COMMENT ON COLUMN public.se_db.name IS 'UNIQUE .';
COMMENT ON COLUMN public.se_db.alphabet IS 'Alphabet used for sequences (AA, DNA...)';


CREATE UNIQUE INDEX public.se_db_name_idx
 ON public.se_db
 ( name );

CREATE TABLE public.se_db_instance (
                id IDENTITY NOT NULL,
                release VARCHAR(50) NOT NULL,
                source_path LONGVARCHAR NOT NULL,
                source_last_modified_time TIMESTAMP NOT NULL,
                se_db_id BIGINT NOT NULL,
                CONSTRAINT se_db_instance_pk PRIMARY KEY (id)
);
COMMENT ON TABLE public.se_db_instance IS 'Unique version of a SE Db.';
COMMENT ON COLUMN public.se_db_instance.release IS 'Version string, if date must be yyyyMMdd .
UNIQUE for given seq_db_id .';
COMMENT ON COLUMN public.se_db_instance.source_path IS 'Can be the pathname of a FASTA file relative to Search Engine file system.';
COMMENT ON COLUMN public.se_db_instance.source_last_modified_time IS 'FASTA file last modified date or SEDbInstance creation timestamp.';


CREATE UNIQUE INDEX public.se_db_instance_idx
 ON public.se_db_instance
 ( se_db_id, release );

CREATE TABLE public.bio_sequence (
                id IDENTITY NOT NULL,
                sequence LONGVARCHAR NOT NULL,
                hash VARCHAR(64) NOT NULL,
                CONSTRAINT bio_sequence_pk PRIMARY KEY (id)
);
COMMENT ON TABLE public.bio_sequence IS 'Proteine sequence.';
COMMENT ON COLUMN public.bio_sequence.id IS 'Auto incremented Id';
COMMENT ON COLUMN public.bio_sequence.sequence IS 'Protein sequence (AA or DNA alphabet) normalized to Upper Case.
Unicity constraint enforced by unicity on "hash" (SHA-256) column.';
COMMENT ON COLUMN public.bio_sequence.hash IS 'SHA-256 hash of normalized (Upper Case) sequence (as ASCII / ISO 8859-1 byte array).
Hash must be UNIQUE .';


CREATE UNIQUE INDEX public.bio_sequence_hash_idx
 ON public.bio_sequence
 ( hash );

CREATE TABLE public.se_db_identifier (
                id IDENTITY NOT NULL,
                value VARCHAR(255) NOT NULL,
                inferred BOOLEAN DEFAULT false NOT NULL,
                se_db_instance_id BIGINT NOT NULL,
                bio_sequence_id BIGINT NOT NULL,
                repository_identifier_id BIGINT,
                CONSTRAINT se_db_identifier_pk PRIMARY KEY (id)
);
COMMENT ON COLUMN public.se_db_identifier.value IS 'UNIQUE for given se_db_instance.';
COMMENT ON COLUMN public.se_db_identifier.inferred IS 'True if this se_db_identifier is inferred by sequence repository service (SE Db source cannot be loaded or protein description does not match).';


CREATE UNIQUE INDEX public.se_db_identifier_idx
 ON public.se_db_identifier
 ( value, se_db_instance_id );

ALTER TABLE public.se_db ADD CONSTRAINT parsing_rule_se_db_fk
FOREIGN KEY (parsing_rule_id)
REFERENCES public.parsing_rule (id)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE public.se_db ADD CONSTRAINT repository_se_db_fk
FOREIGN KEY (repository_id)
REFERENCES public.repository (id)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE public.repository_identifier ADD CONSTRAINT repository_repository_identifier_fk
FOREIGN KEY (repository_id)
REFERENCES public.repository (id)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE public.se_db_identifier ADD CONSTRAINT repository_identifier_se_db_identifier_fk
FOREIGN KEY (repository_identifier_id)
REFERENCES public.repository_identifier (id)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE public.se_db_instance ADD CONSTRAINT se_db_se_db_instance_fk
FOREIGN KEY (se_db_id)
REFERENCES public.se_db (id)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE public.se_db_identifier ADD CONSTRAINT se_db_instance_se_db_identifier_fk
FOREIGN KEY (se_db_instance_id)
REFERENCES public.se_db_instance (id)
ON DELETE NO ACTION
ON UPDATE NO ACTION;

ALTER TABLE public.se_db_identifier ADD CONSTRAINT bio_sequence_se_db_identifier_fk
FOREIGN KEY (bio_sequence_id)
REFERENCES public.bio_sequence (id)
ON DELETE NO ACTION
ON UPDATE NO ACTION;
