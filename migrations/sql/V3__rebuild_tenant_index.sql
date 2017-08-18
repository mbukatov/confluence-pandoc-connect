ALTER TABLE tenant RENAME TO tenant_v2;

CREATE TABLE tenant (
   id  SERIAL PRIMARY KEY
,  key VARCHAR(255) unique not null
,  publicKey TEXT not null
,  oauthClientId TEXT null
,  sharedSecret VARCHAR(512) null
,  baseUrl VARCHAR(512) unique not null
,  productType VARCHAR(50) not null
,  sleep_date TIMESTAMP WITH TIME ZONE
);

INSERT INTO tenant (key, publicKey, oauthClientId, sharedSecret, baseUrl, productType, sleep_date)
SELECT key, publicKey, oauthClientId, sharedSecret, baseUrl, productType, sleep_date FROM tenant_v2;

DROP INDEX tenant_idx;

CREATE INDEX tenant_idx ON tenant (sharedSecret);
