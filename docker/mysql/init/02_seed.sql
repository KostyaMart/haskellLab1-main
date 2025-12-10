INSERT INTO resource_types (name) VALUES
  ('article'), ('dataset'), ('software'), ('presentation')
ON DUPLICATE KEY UPDATE name = VALUES(name);

INSERT INTO authors (full_name, dept) VALUES
  ('Dr. Alice Smith','CS'),
  ('Prof. Bob Brown','Math'),
  ('Dr. Carol Johnson','CS');

INSERT INTO users (username, email, role) VALUES
  ('student1','student1@example.com','student'),
  ('staff1','staff1@example.com','staff'),
  ('admin','admin@example.com','admin')
ON DUPLICATE KEY UPDATE email=VALUES(email), role=VALUES(role);

INSERT INTO resources (title, type_id, abstract, purpose, opened_at, usage_term_days, usage_terms)
VALUES
  ('Intro to Functional Programming', 4, 'Slides about Haskell and FP', 'education','2024-09-01', 365, 'non-commercial use'),
  ('KNU CS Dataset 2023', 2, 'Anonymized dataset for research', 'research', '2023-01-10', 730, 'citation required');

INSERT INTO resource_authors (resource_id, author_id)
VALUES (1,1),(1,3),(2,2);

INSERT INTO urls (resource_id, url, kind)
VALUES (1, 'https://example.org/fp-slides', 'homepage'),
       (2, 'https://example.org/knu-dataset-2023', 'homepage');

INSERT INTO usage_events (resource_id, user_id, action)
VALUES (1,1,'view'),(1,1,'download'),(1,2,'view'),
       (2,1,'view'),(2,3,'like');
