
-------------------------------------------------------------------------------
-- Table definitions

-- TODO: add body or something?
CREATE TABLE hs_lemma (
  name TEXT NOT NULL PRIMARY KEY,
  indvars integer[] NOT NULL DEFAULT '{}',
  body TEXT NOT NULL
);

-- Which lemmas use other lemmas in their proofs
CREATE TABLE hs_lemma_using (
  lemma TEXT NOT NULL REFERENCES hs_lemma(name),
  uses TEXT NOT NULL REFERENCES hs_lemma(name),
  PRIMARY KEY (lemma, uses)
);

-- The features extracted from a lemma
CREATE TABLE hs_lemma_feature (
  lemma TEXT NOT NULL REFERENCES hs_lemma(name),
  feature TEXT NOT NULL
);

GRANT ALL PRIVILEGES ON hs_lemma TO hipspecuser;
GRANT ALL PRIVILEGES ON hs_lemma_using TO hipspecuser;
GRANT ALL PRIVILEGES ON hs_lemma_feature TO hipspecuser;

-------------------------------------------------------------------------------
-- Dummy data
/*
-- Lemmas
INSERT INTO hs_lemma VALUES('lemma-0', '{0}');
INSERT INTO hs_lemma VALUES('lemma-1', '{1}');
INSERT INTO hs_lemma VALUES('lemma-2', '{}');
INSERT INTO hs_lemma VALUES('lemma-3', '{0}');

-- Which lemmas they used in their proofs (not used atm)
INSERT INTO hs_lemma_using VALUES('lemma-0', 'lemma-1');

-- Features for lemmas
INSERT INTO hs_lemma_feature VALUES('lemma-0', '++');
INSERT INTO hs_lemma_feature VALUES('lemma-0', 'List');
INSERT INTO hs_lemma_feature VALUES('lemma-1', '++');
INSERT INTO hs_lemma_feature VALUES('lemma-1', 'rev');
INSERT INTO hs_lemma_feature VALUES('lemma-1', 'List');
INSERT INTO hs_lemma_feature VALUES('lemma-2', 'Nat');
INSERT INTO hs_lemma_feature VALUES('lemma-2', '+');
*/