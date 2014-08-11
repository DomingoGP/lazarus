
CREATE GENERATOR GEN_DEADLYMUSHROOMS_ID;

CREATE TABLE DEADLYMUSHROOMS
(
  ID INTEGER NOT NULL,
  SCIENTIFIC_NAME VARCHAR(200) NOT NULL,
  COMMON_NAME VARCHAR(200),
  "Order" VARCHAR(200),
  GENUS VARCHAR(200),
  NOTES BLOB SUB_TYPE 1,
  PICTURE BLOB SUB_TYPE 0,
  IMAGE_LINK VARCHAR(1000),
  CONSTRAINT CPKDM_1 PRIMARY KEY (ID)
);

SET TERM ^ ;
CREATE TRIGGER DEADLYMUSHROOMS_BI FOR DEADLYMUSHROOMS ACTIVE
BEFORE INSERT POSITION 0
AS
DECLARE VARIABLE tmp DECIMAL(18,0);
BEGIN
  IF (NEW.ID IS NULL) THEN
    NEW.ID = GEN_ID(GEN_DEADLYMUSHROOMS_ID, 1);
  ELSE
  BEGIN
    tmp = GEN_ID(GEN_DEADLYMUSHROOMS_ID, 0);
    if (tmp < new.ID) then
      tmp = GEN_ID(GEN_DEADLYMUSHROOMS_ID, new.ID-tmp);
  END
END^
SET TERM ; ^

UPDATE RDB$GENERATORS set
  RDB$DESCRIPTION = 'Generator (sequence) that gives a new unique ID. The trigger for the mushrooms table uses this.'
  where RDB$GENERATOR_NAME = 'GEN_DEADLYMUSHROOMS_ID';
UPDATE RDB$RELATION_FIELDS set RDB$DESCRIPTION = 'Unique ID; primary key'  where RDB$FIELD_NAME = 'ID' and RDB$RELATION_NAME = 'DEADLYMUSHROOMS';
UPDATE RDB$RELATION_FIELDS set RDB$DESCRIPTION = 'Scientific name (often Latin) of the mushroom'  where RDB$FIELD_NAME = 'SCIENTIFIC_NAME' and RDB$RELATION_NAME = 'DEADLYMUSHROOMS';
UPDATE RDB$RELATION_FIELDS set RDB$DESCRIPTION = 'Common or garden name for the mushroom'  where RDB$FIELD_NAME = 'COMMON_NAME' and RDB$RELATION_NAME = 'DEADLYMUSHROOMS';
UPDATE RDB$RELATION_FIELDS set RDB$DESCRIPTION = 'Free-form notes about the mushroom'  where RDB$FIELD_NAME = 'NOTES' and RDB$RELATION_NAME = 'DEADLYMUSHROOMS';
UPDATE RDB$RELATION_FIELDS set RDB$DESCRIPTION = 'BLOB containing picture data'  where RDB$FIELD_NAME = 'PICTURE' and RDB$RELATION_NAME = 'DEADLYMUSHROOMS';
UPDATE RDB$RELATION_FIELDS set RDB$DESCRIPTION = 'Location of image in file system'  where RDB$FIELD_NAME = 'IMAGE_LINK' and RDB$RELATION_NAME = 'DEADLYMUSHROOMS';
UPDATE RDB$RELATIONS set
RDB$DESCRIPTION = 'Table of mushroom names and images'
where RDB$RELATION_NAME = 'DEADLYMUSHROOMS';
UPDATE RDB$TRIGGERS set
  RDB$DESCRIPTION = 'Trigger that gives a new unique ID on each insert in the mushroom table if an ID hasn''t been provided in the SQL. Emulates the autonumber/autoincrement functionality of other databases but is more powerful.'
  where RDB$TRIGGER_NAME = 'DEADLYMUSHROOMS_BI';
GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON DEADLYMUSHROOMS TO  SYSDBA WITH GRANT OPTION;
GRANT DELETE, INSERT, REFERENCES, SELECT, UPDATE
 ON DEADLYMUSHROOMS TO  PUBLIC;
